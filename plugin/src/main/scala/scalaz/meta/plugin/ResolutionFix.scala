package scalaz.meta.plugin

import scalaz.meta.plugin.Definitions

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.{ Global, SubComponent }

abstract class ResolutionFix {
  val global: Global
  val scalazDefns: Definitions { val global: ResolutionFix.this.global.type }

  import global._

  val newAnalyzer = new NewAnalyzer()

  final class NewAnalyzer extends {
    val global: ResolutionFix.this.global.type = ResolutionFix.this.global
  } with Analyzer {
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
        val result = search.allImplicits.find(_.isSuccess).getOrElse(search.bestImplicit)
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

  def getGlobalPhasesSet =
    valGetter[Global, mutable.HashSet[SubComponent]]("phasesSet")
  def getGlobalPhasesDescMap =
    valGetter[Global, mutable.Map[SubComponent, String]]("phasesDescMap")
  def setGlobalAnalyzer =
    valSetter[Global, Analyzer]("analyzer")

  def init(): Unit = {
    val phases     = getGlobalPhasesSet(global)
    val phaseDescs = getGlobalPhasesDescMap(global)

    val oldTyper = phases.find(s => s.phaseName == "typer").get
    val oldDesc  = phaseDescs(oldTyper)

    phases.remove(oldTyper)
    phaseDescs.remove(oldTyper)
    setGlobalAnalyzer(global, newAnalyzer)
    phases.add(newAnalyzer.typerFactory)
    phaseDescs.put(newAnalyzer.typerFactory, oldDesc)
  }
}
