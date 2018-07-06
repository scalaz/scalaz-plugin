package scalaz.meta.plugin

import scala.reflect.ClassTag
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.typechecker.AnalyzerPlugins

class MetaPlugin(val global: Global) extends Plugin { plugin =>
  val name        = "scalaz"
  val description = "scalaz"

  val scalazDefns = new {
    val global: plugin.global.type = plugin.global
  } with Definitions

  scalazDefns.init()

  object resolutionFix extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with ResolutionFix

  if (!options.contains("-resolution")) resolutionFix.init()

  object sufficiency extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with SufficiencyChecker

  object orphanChecker extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with OrphanChecker

  object polymorphicFunctionOptimizer extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with PolymorphicFunctionOptimizer

  object newtypes extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with Newtypes

  if (!options.contains("-newtypes")) {
    global.analyzer.addAnalyzerPlugin(newtypes.MyTransformer)
    global.analyzer.addMacroPlugin(newtypes.MyTransformer)
  }

  val components: List[PluginComponent] = List[(String, PluginComponent)](
    "minimal" -> sufficiency,
    "orphans" -> orphanChecker,
    "polyopt" -> polymorphicFunctionOptimizer
  ).flatMap {
    case (opt, phf) =>
      if (options.contains(s"-$opt")) None
      else Some(phf)
  }
}
