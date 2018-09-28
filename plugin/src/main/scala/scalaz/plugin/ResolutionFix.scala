package scalaz.plugin

import java.lang.reflect.{ Field, Modifier }

import miniz._

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.doc.{ ScaladocAnalyzer, ScaladocGlobal }
import scala.tools.nsc.interactive.InteractiveAnalyzer
import scala.tools.nsc.typechecker.{ Analyzer, AnalyzerPlugins, Contexts, Macros }
import scala.tools.nsc.interactive.{ Global => InteractiveGlobal }

object FieldBuster {

  /** Get all superclasses of the class. */
  def getSuperclasses[T](cls: Class[T]): List[Class[_]] = {
    val sc = cls.getSuperclass
    if (sc == null) Nil
    else sc :: getSuperclasses(sc)
  }

  /** Get all fields in the class and its superclasses. */
  def getAllClassFields[T](cls: Class[T]): List[Field] =
    (cls :: getSuperclasses(cls)).flatMap(c => c.getDeclaredFields)

  final class Ref(val value: AnyRef) {
    override def equals(that: Any): Boolean =
      if (!that.isInstanceOf[Ref]) false
      else that.asInstanceOf[Ref].value eq this.value

    override def hashCode(): Int =
      System.identityHashCode(value)

    def project[Z](alg: (Select, () => Ref, Ref => Unit) => Z): JvmObject[Z] = value match {
      case null => JvmObject.Null

      case coll: Array[AnyRef] =>
        JvmObject.Array(
          coll.indices
            .map(
              index =>
                alg(Select.Index(index), { case () => new Ref(coll(index)) }, { r =>
                  coll(index) = r.value
                })
            )
            .toList
        )

      case _ =>
        JvmObject.Dict(
          value.getClass.getName,
          getAllClassFields(value.getClass)
            .filter(f => !f.getType.isPrimitive)
            .filter(f => !Modifier.isStatic(f.getModifiers))
            .map { f =>
              f.setAccessible(true)
              val declaringClass = f.getDeclaringClass.getName
              val fieldName      = f.getName
              val fullName       = FullFieldName(declaringClass, fieldName)

              fullName -> alg(Select.Field(fullName), { case () => new Ref(f.get(value)) }, { r =>
                f.set(value, r.value)
              })
            }
            .toMap
        )
    }
  }

  final case class FullFieldName(declaringClass: String, fieldName: String)

  sealed trait JvmObject[+R]
  object JvmObject {
    final case object Null                       extends JvmObject[Nothing]
    final case class Array[R](contents: List[R]) extends JvmObject[R]
    final case class Dict[R](className: String, contents: Map[FullFieldName, R])
        extends JvmObject[R]
  }

  sealed abstract class Select
  object Select {
    final case class Field(name: FullFieldName) extends Select
    final case class Index(index: Int)          extends Select

    sealed abstract class Error
    object Error {
      final case object NullExpectedArray                                  extends Error
      final case object NullExpectedDict                                   extends Error
      final case class IndexOutOfBounds(i: Int, size: Int)                 extends Error
      final case object ArrayExpectedDict                                  extends Error
      final case class DictExpectedArray(className: String)                extends Error
      final case class NoSuchField(className: String, name: FullFieldName) extends Error
    }
  }

  def select[R](j: JvmObject[R], s: Select): Either[Select.Error, R] = {
    import FieldBuster.{ Select => S }
    import Select.Error._

    (j, s) match {
      case (JvmObject.Null, S.Index(_)) => -\/(NullExpectedArray)
      case (JvmObject.Null, S.Field(_)) => -\/(NullExpectedDict)
      case (JvmObject.Array(c), S.Index(i)) =>
        val s = c.length
        if (i >= 0 && i < s) \/-(c(i))
        else -\/(IndexOutOfBounds(i, s))
      case (JvmObject.Array(_), S.Field(_))   => -\/(ArrayExpectedDict)
      case (JvmObject.Dict(n, _), S.Index(_)) => -\/(DictExpectedArray(n))
      case (JvmObject.Dict(cn, c), S.Field(n)) =>
        c.get(n) match {
          case None    => -\/(NoSuchField(cn, n))
          case Some(x) => \/-(x)
        }
    }
  }

  sealed abstract class Origin
  object Origin {
    final case object Global extends Origin
    final case object OldRef extends Origin
    final case object NewRef extends Origin
  }

  final case class Path(origin: Origin, parts: Nel[Select]) {
    def /(select: Select): Path = Path(origin, select :: parts)
  }

  // FIXME: EVIL, but used only in ResolutionFixDb
  def parsePath(s: String): Path = {
    val arr = s.split('/')
    val origin = arr(0) match {
      case "_root_" => Origin.Global
      case "_old_"  => Origin.OldRef
      case "_new_"  => Origin.NewRef
    }
    val parts = arr.tail.map { s =>
      val i = s.indexOf('@')
      if (i >= 0) {
        val fn = s.substring(0, i)
        val cn = s.substring(i + 1)
        Select.Field(FullFieldName(cn, fn))
      } else {
        val index = s.substring(2, s.length - 1).toInt
        Select.Index(index)
      }
    }.toList.reverse.toNel.get
    Path(origin, parts)
  }

  def formatPath(path: Path): String = {
    val o = path.origin match {
      case Origin.Global => "_root_"
      case Origin.NewRef => "_new_"
      case Origin.OldRef => "_old_"
    }
    val p = path.parts.toList.map {
      case Select.Field(FullFieldName(dc, fn)) => "/" + fn + "@" + dc
      case Select.Index(index)                 => "/" + index + ""
    }
    o + p.reverse.mkString("")
  }

  final case class PathFollowError(origin: Origin,
                                   rest: List[Select],
                                   current: Select,
                                   passed: List[Select],
                                   error: Select.Error)

  def follow(root: Ref,
             oldRef: Ref,
             newRef: Ref,
             path: Path): PathFollowError \/ (() => Ref, Ref => Unit) = {
    val start = path.origin match {
      case Origin.Global => root
      case Origin.NewRef => newRef
      case Origin.OldRef => oldRef
    }

    path.parts.foldRightEitherH[PathFollowError, (() => Ref, Ref => Unit)](
      {
        case (hist, s) =>
          val obj = start.project { case (_, get, set) => (get, set) }
          select(obj, s).leftMap(PathFollowError(path.origin, hist, s, Nil, _))
      }, {
        case (hist, rest, s, (get, _)) =>
          val obj = get().project { case (_, getf, setf) => (getf, setf) }
          select(obj, s).leftMap(PathFollowError(path.origin, hist, s, rest.toList, _))
      }
    )
  }

  def replaceAll(globalRef: Ref, oldRef: Ref, newRef: Ref): List[Path] = {
    val visited = mutable.Set.empty[Ref]
    val queue   = new java.util.ArrayDeque[(Ref, Origin \/ Path)]
    val updated = List.newBuilder[Path]

    def visitOrigin(o: Origin, r: Ref): Unit = {
      visited += r
      queue.addLast((r, -\/(o)))
    }

    def append(p: Origin \/ Path, s: Select): Path = p match {
      case -\/(origin) => Path(origin, Nel.of(s))
      case \/-(path)   => path / s
    }

    visitOrigin(Origin.Global, globalRef)
    visitOrigin(Origin.OldRef, oldRef)
    visitOrigin(Origin.NewRef, newRef)

    while (queue.size() > 0) {
      val (node, path) = queue.pop()

      node.project {
        case (select, get, set) =>
          val value = get()
          val fpath = append(path, select)

          if (value == oldRef) {
            updated += fpath
            set(newRef)
          }

          if (!visited.contains(value)) {
            visited += value
            queue.addLast((value, \/-(fpath)))
          }
      }
    }

    updated.result()
  }
}

abstract class ResolutionFix {
  val global: Global
  val scalazDefns: Definitions { val global: ResolutionFix.this.global.type }

  import global.{ Select => _, _ }

  trait AnalyzerMixin extends Analyzer {
    import global._

    var oldClassLoader: () => ClassLoader = _

    lazy val TypeclassClass: ClassSymbol = rootMirror.getRequiredClass("scalaz.meta.Typeclass")

    def selectScore(tree: Tree): Option[Int] = tree match {
      case TypeApply(fun, args) => selectScore(fun)
      case Select(qualifier, _) => selectScore(qualifier).map(_ + 1)
      case Ident(a)             => Some(1)
      case This(_)              => Some(0)
      case Super(qual, _)       => selectScore(qual)
      case _                    => None
    }

    def chooseBestInstance(tree: Tree, all: List[SearchResult]): SearchResult =
      if (all.isEmpty) SearchFailure
      else
        all.minBy { r =>
          val s = if (r.isSuccess) 0 else 1
          val l = if (r.tree.symbol.isParameter) 0 else 1
          val d = selectScore(r.tree).getOrElse(100)

          (s, l, d)
        }

    override def inferImplicit(
      tree: Tree,
      pt: Type,
      reportAmbiguous: Boolean,
      isView: Boolean,
      context: Context,
      saveAmbiguousDivergent: Boolean,
      pos: Position
    ): SearchResult =
      if (pt <:< TypeclassClass.tpe) {
        // Note that the isInvalidConversionTarget seems to make a lot more sense right here, before all the
        // work is performed, than at the point where it presently exists.
        val shouldPrint = printTypings && context.undetparams.nonEmpty
        if (shouldPrint)
          typingStack.printTyping(
            tree,
            "typing typeclass: %s %s".format(tree, context.undetparamsString)
          )
        val implicitSearchContext = context.makeImplicit(reportAmbiguous)

        val search = new ImplicitSearch(tree, pt, isView, implicitSearchContext, pos)
        pluginsNotifyImplicitSearch(search)
        val result = chooseBestInstance(tree, search.allImplicits)
        pluginsNotifyImplicitSearchResult(result)

        if (result.isFailure && saveAmbiguousDivergent && implicitSearchContext.reporter.hasErrors)
          implicitSearchContext.reporter.propagateImplicitTypeErrorsTo(context.reporter)

        context.undetparams =
          ((context.undetparams ++ result.undetparams) filterNot result.subst.from.contains).distinct

        result
      } else {
        super.inferImplicit(tree, pt, reportAmbiguous, isView, context, saveAmbiguousDivergent, pos)
      }
  }

  final class NewInteractiveAnalyzer extends {
    val global: InteractiveGlobal = ResolutionFix.this.global.asInstanceOf[InteractiveGlobal]
  } with InteractiveAnalyzer with AnalyzerMixin {
    override def findMacroClassLoader(): ClassLoader = oldClassLoader()
  }

  final class NewScaladocAnalyzer extends {
    val global: ScaladocGlobal = ResolutionFix.this.global.asInstanceOf[ScaladocGlobal]
  } with ScaladocAnalyzer with AnalyzerMixin {
    override def findMacroClassLoader(): ClassLoader = oldClassLoader()
  }

  final class NewAnalyzer extends {
    val global: ResolutionFix.this.global.type = ResolutionFix.this.global
  } with AnalyzerMixin {
    override def findMacroClassLoader(): ClassLoader = oldClassLoader()
  }

  import FieldBuster._

  def getMacroClassLoader(analyzer: Analyzer): ClassLoader = {
    val method = analyzer.getClass.getMethod("findMacroClassLoader")
    method.setAccessible(true)
    method.invoke(analyzer).asInstanceOf[ClassLoader]
  }

  def copyState(old: Analyzer, neu: Analyzer): Unit = {
    import FieldBuster.getAllClassFields

    def copyField(old: AnyRef, neu: AnyRef, name: String): Unit = {
      val oldField = getAllClassFields(old.getClass).find(_.getName.endsWith(name)).get
      oldField.setAccessible(true)
      val field = getAllClassFields(neu.getClass).find(_.getName.endsWith(name)).get
      field.setAccessible(true)
      field.set(neu, oldField.get(old))
    }

    copyField(old, neu, "macroPlugins")
    copyField(old, neu, "analyzerPlugins")
  }

  def formatPathError(p: PathFollowError): String = p match {
    case PathFollowError(origin, rest, current, passed, error) =>
      val o = origin match {
        case Origin.Global => "_root_"
        case Origin.NewRef => "_new_"
        case Origin.OldRef => "_old_"
      }
      val p = passed.map {
        case Select.Field(FullFieldName(cn, fn)) => "/" + fn + "@" + cn
        case Select.Index(index)                 => "/" + index
      }
      val c = current match {
        case Select.Field(FullFieldName(cn, fn)) => "/{" + fn + "@" + cn + "}"
        case Select.Index(index)                 => "/{" + index + "}"
      }
      val r = rest.map {
        case Select.Field(FullFieldName(cn, fn)) => "/" + fn + "@" + cn
        case Select.Index(index)                 => "/" + index
      }
      val fullPath = o + p.reverse.mkString("") + c + r.reverse.mkString("")
      val errorDesc = error match {
        case Select.Error.NullExpectedArray => "expected an array, found null"
        case Select.Error.NullExpectedDict  => "expected an object, found null"
        case Select.Error.IndexOutOfBounds(i, size) =>
          s"index $i was out of bounds in an array of size $size"
        case Select.Error.ArrayExpectedDict => "expected an object, found an array"
        case Select.Error.DictExpectedArray(n) =>
          s"expected an array, found an object with class name $n"
        case Select.Error.NoSuchField(className, FullFieldName(cls, fieldName)) =>
          s"expected an object with field named $fieldName declared in class $cls, " +
            s"found an object with class name $className"
      }

      s"Could not follow $fullPath: $errorDesc"
  }

  def init(): Unit =
    try {
      val oldAnalyzer: Analyzer = global.analyzer

      val newAnalyzer = oldAnalyzer match {
        case _: NewAnalyzer            => return
        case _: NewInteractiveAnalyzer => return
        case i: InteractiveAnalyzer    => new NewInteractiveAnalyzer()
        case i: ScaladocAnalyzer       => new NewScaladocAnalyzer()
        case i: Analyzer               => new NewAnalyzer()
      }

      val globalName   = global.getClass.getName
      val analyzerName = oldAnalyzer.getClass.getName

      val globalRef = new Ref(global)
      val newRef    = new Ref(newAnalyzer)
      val oldRef    = new Ref(oldAnalyzer)

      ResolutionFixDb.db.get((globalName, analyzerName)) match {
        case None =>
          Console.err.println((globalName, analyzerName))
          for (p <- FieldBuster.replaceAll(globalRef, oldRef, newRef)) {
            Console.err.println(s"${formatPath(p)}")
          }

        case Some(rewrites) =>
          for (path <- rewrites) {
            // println(s"${formatPath(path)}")
            val result = FieldBuster.follow(globalRef, oldRef, newRef, path)

            result match {
              case -\/(e) =>
                Console.err.println(formatPathError(e))

              case \/-(get /\ set) =>
                if (get() == oldRef) {
                  set(newRef)
                } else {
                  Console.err.println(s"Expected ${formatPath(path)} to point at the old analyzer.")
                }
            }
          }
      }

      copyState(oldAnalyzer, newAnalyzer)
      newAnalyzer.oldClassLoader = () => getMacroClassLoader(oldAnalyzer)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
}
