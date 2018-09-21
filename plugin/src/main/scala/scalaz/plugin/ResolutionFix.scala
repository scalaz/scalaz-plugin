package scalaz.plugin

import java.lang.reflect.{Field, Modifier}

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.interactive.InteractiveAnalyzer
import scala.tools.nsc.typechecker.{Analyzer, AnalyzerPlugins, Contexts, Macros}
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}

object FieldBuster {
  def getSuperclasses[T](cls: Class[T]): List[Class[_]] = {
    val sc = cls.getSuperclass
    if (sc == null) Nil
    else sc :: getSuperclasses(sc)
  }

  def getAllClassFields[T](cls: Class[T]): List[Field] =
    (cls :: getSuperclasses(cls)).flatMap(c => c.getDeclaredFields)

  final class FieldLens[U](val get: () => U, val set: U => Unit)
  object FieldLens {
    def get(root: AnyRef, oldRef: AnyRef, newRef: AnyRef, path: Path): FieldLens[AnyRef] =
      path.fold[FieldLens[AnyRef]](
        {
          case Root   => new FieldLens[AnyRef](() => root, x => ???)
          case OldRef => new FieldLens[AnyRef](() => oldRef, x => ???)
          case NewRef => new FieldLens[AnyRef](() => newRef, x => ???)
        },
        (s, z) =>
          s match {
            case SelectIndex(index) =>
              val ref = z.get().asInstanceOf[Array[AnyRef]]
              new FieldLens[AnyRef](() => ref(index), x => { ref(index) = x })
            case SelectField(name) =>
              val ref = z.get()
              // println(s"$ref $name")
              val f = getAllClassFields(ref.getClass).find(_.getName == name).get
              f.setAccessible(true)
              new FieldLens[AnyRef](() => f.get(ref), x => f.set(ref, x))
        }
      )
  }

  def getFields(ref: AnyRef): List[(Select, FieldLens[AnyRef])] =
    if (ref eq null) Nil
    else
      ref match {
        case coll: Array[AnyRef] =>
          coll.toList.zipWithIndex.map {
            case (_, i) =>
              SelectIndex(i) -> new FieldLens[AnyRef](() => coll(i), x => { coll(i) = x })
          }

        case _ =>
          getAllClassFields(ref.getClass)
            .filter(f => !f.getType.isPrimitive)
            .map { f =>
              f.setAccessible(true)
              SelectField(f.getName) -> new FieldLens[AnyRef](() => f.get(ref), x => f.set(ref, x))
            }
      }

  sealed abstract class Select
  final case class SelectField(name: String) extends Select {
    override def toString: String = "SelectField(\"" + name + "\")"
  }
  final case class SelectIndex(index: Int) extends Select

  sealed abstract class Origin
  final case object Root   extends Origin
  final case object OldRef extends Origin
  final case object NewRef extends Origin

  final case class Path(origin: Origin, parts: List[Select]) {
    def ::(select: Select): Path = Path(origin, select :: parts)
    def fold[Z](o: Origin => Z, part: (Select, Z) => Z): Z =
      parts.foldRight(o(origin))(part)
  }

  final class Wrapper(val value: AnyRef) {
    override def equals(that: Any): Boolean =
      if (!that.isInstanceOf[Wrapper]) false
      else that.asInstanceOf[Wrapper].value eq this.value

    override def hashCode(): Int =
      System.identityHashCode(value)
  }

  def replaceAll(root: AnyRef, oldRef: AnyRef, newRef: AnyRef): List[Path] = {
    val visited = mutable.HashMap.empty[Wrapper, Path]
    val queue   = new java.util.ArrayDeque[(AnyRef, Path)]

    val updated = mutable.ArrayBuffer.empty[Path]

    def visit(value: AnyRef, path: Path): Unit =
      if (!(value eq null)) {
        val valueW = new Wrapper(value)
        if (!visited.contains(valueW)) {
          visited += (valueW -> path)
          queue.addLast((value, path))
        }
      }

    visit(root, Path(Root, Nil))
    visit(oldRef, Path(OldRef, Nil))
    visit(newRef, Path(NewRef, Nil))

    while (queue.size() > 0) {
      val (node, path) = queue.pop()

      getFields(node).foreach {
        case (action, field) =>
          val value: AnyRef = field.get()
          if (value eq oldRef) {
            updated += action :: path
            field.set(newRef)
          }

          visit(value, action :: path)
      }
    }

    updated.toList
  }
}

abstract class ResolutionFix {
  val global: Global
  val scalazDefns: Definitions { val global: ResolutionFix.this.global.type }

  import global._

  trait AnalyzerMixin extends Analyzer {
    import global._

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
    var oldClassLoader: () => ClassLoader = _
    override def findMacroClassLoader(): ClassLoader = oldClassLoader()
  }

  final class NewAnalyzer extends {
    val global: ResolutionFix.this.global.type = ResolutionFix.this.global
  } with AnalyzerMixin {
    var oldClassLoader: () => ClassLoader = _
    override def findMacroClassLoader(): ClassLoader = oldClassLoader()
  }

  import FieldBuster._

  val nonInteractiveCommands = List(
    Path(Root, List(SelectField("analyzer"))),
    Path(Root, List(SelectField("analyzer"), SelectField("delambdafy$module"))),
    Path(Root, List(SelectField("$outer"), SelectField("lastSeenContext"))),
    Path(OldRef, List(SelectField("$outer"), SelectField("namerFactory$module"))),
    Path(OldRef, List(SelectField("$outer"), SelectField("typerFactory$module"))),
    Path(OldRef, List(SelectField("$outer"), SelectField("NoImplicitInfo"))),
    Path(OldRef, List(SelectField("$outer"), SelectField("NoContext$module"))),
    Path(OldRef, List(SelectField("$outer"), SelectField("Context$module"))),
    Path(Root,
         List(SelectField("$outer"),
              SelectField("scala$tools$nsc$typechecker$Contexts$Context$$_reporter"),
              SelectField("lastSeenContext"))),
  ).reverse

  val interactiveCommands = List(
    Path(Root,List(SelectField("analyzer"))),
    Path(Root,List(SelectField("analyzer"), SelectField("delambdafy$module"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("namerFactory$module"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("typerFactory$module"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("NoImplicitInfo"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("NoContext$module"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("Context$module"))),
    Path(OldRef,List(SelectField("$outer"), SelectField("scala$tools$nsc$typechecker$Contexts$Context$$_reporter"), SelectField("NoContext$module"))),
  ).reverse

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

  def init(): Unit =
    try {
      val oldAnalyzer: Analyzer = global.analyzer

      oldAnalyzer match {
        case _: NewAnalyzer => ()
        case _: NewInteractiveAnalyzer => ()

        case i: InteractiveAnalyzer =>
//          for (p <- FieldBuster.replaceAll(global, global.analyzer, new NewInteractiveAnalyzer)) {
//            println(s"$p,")
//          }
          val newAnalyzer = new NewInteractiveAnalyzer()
          for (c <- interactiveCommands) {
            FieldLens.get(global, oldAnalyzer, newAnalyzer, c).set(newAnalyzer)
          }
          copyState(oldAnalyzer, newAnalyzer)
          newAnalyzer.oldClassLoader = () => getMacroClassLoader(oldAnalyzer)

        case ni: Analyzer =>
          val newAnalyzer = new NewAnalyzer()
          for (c <- nonInteractiveCommands) {
            FieldLens.get(global, oldAnalyzer, newAnalyzer, c).set(newAnalyzer)
          }
          copyState(oldAnalyzer, newAnalyzer)
          newAnalyzer.oldClassLoader = () => getMacroClassLoader(oldAnalyzer)
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
}
