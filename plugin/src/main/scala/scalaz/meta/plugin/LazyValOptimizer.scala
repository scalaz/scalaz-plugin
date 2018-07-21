package scalaz.meta.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal

abstract class LazyValOptimizer extends PluginComponent with Transform with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: LazyValOptimizer.this.global.type }

  import global._
  import scalazDefns.{ global => _ }

  override val phaseName: String       = "scalaz-lazyvalopt"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree =
      //println(global.showRaw(tree))
      try {
        tree match {
          case cd @ ClassDef(_, _, _, tmpl @ Template(_, _, body)) =>
            // check body has lazy val
            // optimize it
            tree
          case mod @ ModuleDef(_, _, tmpl @ Template(_, _, body)) =>
            // check body has lazy val
            // optimize it
            tree
          case _ => super.transform(tree)
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          super.transform(tree)
      }
  }

}
