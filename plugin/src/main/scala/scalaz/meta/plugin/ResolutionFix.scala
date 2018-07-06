package scalaz.meta.plugin

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.tools.nsc.typechecker.{ Analyzer, AnalyzerPlugins }
import scala.tools.nsc.{ Global, SubComponent }
import scala.util.control.NonFatal

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

  def valSetter[T: ClassTag, U](name: String): (T, U) => Unit = {
    val cls   = implicitly[ClassTag[T]].runtimeClass
    val field = cls.getDeclaredField(name)
    field.setAccessible(true)
    (t, u) =>
      field.set(t, u)
  }

  def valGetter[T: ClassTag, U](name: String): T => U = {
    val cls   = implicitly[ClassTag[T]].runtimeClass
    val field = cls.getDeclaredField(name)
    field.setAccessible(true)
    t =>
      field.get(t).asInstanceOf[U]
  }

  def set[U](obj: AnyRef, name: String, value: U): Unit = {
    val cls   = obj.getClass
    val field = cls.getDeclaredField(name)
    field.setAccessible(true)
    field.set(obj, value)
  }

  def getGlobalPhasesSet =
    valGetter[Global, mutable.HashSet[SubComponent]]("phasesSet")
  def getGlobalPhasesDescMap =
    valGetter[Global, mutable.Map[SubComponent, String]]("phasesDescMap")
  def setGlobalAnalyzer =
    valSetter[Global, Analyzer]("analyzer")

  def init(): Unit =
    try {
      val phases     = getGlobalPhasesSet(global)
      val phaseDescs = getGlobalPhasesDescMap(global)

      setGlobalAnalyzer(global, newAnalyzer)

      val oldNamer     = phases.find(s => s.phaseName == "namer").get
      val oldNamerDesc = phaseDescs(oldNamer)
      phases.remove(oldNamer)
      phaseDescs.remove(oldNamer)
      phases.add(newAnalyzer.namerFactory)
      phaseDescs.put(newAnalyzer.namerFactory, oldNamerDesc)

      val oldTyper     = phases.find(s => s.phaseName == "typer").get
      val oldTyperDesc = phaseDescs(oldTyper)
      phases.remove(oldTyper)
      phaseDescs.remove(oldTyper)
      phases.add(newAnalyzer.typerFactory)
      phaseDescs.put(newAnalyzer.typerFactory, oldTyperDesc)
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
    }
}
