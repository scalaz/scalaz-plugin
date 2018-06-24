package scalaz.meta.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._

class MetaPlugin(val global: Global) extends Plugin { plugin =>
  val name        = "scalaz"
  val description = "scalaz"

  val scalazDefns = new {
    val global: plugin.global.type = plugin.global
  } with Definitions

  scalazDefns.init()

  val resolutionFix = new {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with ResolutionFix

  resolutionFix.init()

  object sufficiency extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with SufficiencyChecker

  object orphanChecker extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with OrphanChecker

  val components = List(sufficiency, orphanChecker)
}