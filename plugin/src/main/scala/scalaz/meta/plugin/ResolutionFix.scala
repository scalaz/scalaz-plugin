package scalaz.meta.plugin

import java.lang.reflect.{ Field, Modifier }

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Analyzer

object FieldBuster {
  def getSuperclasses[T](cls: Class[T]): List[Class[_]] = {
    val sc = cls.getSuperclass
    if (sc == null) Nil
    else sc :: getSuperclasses(sc)
  }

  def getAllClassFields[T](cls: Class[T]): List[Field] =
    (cls :: getSuperclasses(cls)).flatMap(c => c.getDeclaredFields)

  final class FieldLens[U](val get: () => U, val set: U => Unit)

  def getFields(ref: AnyRef): List[FieldLens[AnyRef]] =
    if (ref eq null) Nil
    else
      ref match {
        case array: Array[AnyRef] =>
          array.toList.filter(_ != null).zipWithIndex.map {
            case (r, i) =>
              new FieldLens[AnyRef](() => r, x => { array(i) = x })
          }
        case _ =>
          getAllClassFields(ref.getClass)
            .filter(f => !Modifier.isStatic(f.getModifiers))
            .filter(f => !f.getType.isPrimitive)
            .map { f =>
              f.setAccessible(true)
              new FieldLens[AnyRef](() => f.get(ref), x => f.set(ref, x))
            }
      }

  type Path = List[String]

  final class Wrapper(val value: AnyRef) {
    override def equals(that: Any): Boolean =
      if (!that.isInstanceOf[Wrapper]) false
      else that.asInstanceOf[Wrapper].value eq this.value

    override def hashCode(): Int =
      System.identityHashCode(value)
  }

  def replaceAll(root: AnyRef, oldRef: AnyRef, newRef: AnyRef): Unit = {
    val visited = mutable.HashSet.empty[Wrapper]
    val queue   = new java.util.ArrayDeque[AnyRef]

    def visit(value: AnyRef): Unit =
      if (!(value eq null)) {
        val valueW = new Wrapper(value)
        if (!visited.contains(valueW)) {
          visited.add(valueW)
          queue.addLast(value)
        }
      }

    visit(root)
    visit(oldRef)
    visit(newRef)

    while (queue.size() > 0) {
      val node = queue.pop()

      getFields(node).foreach { field =>
        val value: AnyRef = field.get()
        if (value eq oldRef) field.set(newRef)
        visit(value)
      }
    }
  }
}

abstract class ResolutionFix {
  val global: Global
  val scalazDefns: Definitions { val global: ResolutionFix.this.global.type }

  import global._

  val newAnalyzer = new NewAnalyzer()

  final class NewAnalyzer extends {
    val global: ResolutionFix.this.global.type = ResolutionFix.this.global
  } with Analyzer {
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
      if (pt <:< scalazDefns.TypeclassClass.tpe) {
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

  def init(): Unit =
    try {
      FieldBuster.replaceAll(global, global.analyzer, newAnalyzer)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
}
