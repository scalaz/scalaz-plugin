package scalaz.meta.plugin


import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.{Global, plugins}

abstract class OrphanChecker extends plugins.PluginComponent with Transform with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions {val global: OrphanChecker.this.global.type}

  import global._, scalazDefns.{global => _, _}

  override val phaseName: String                        = "scalaz-orphans"
  override val runsAfter: List[String]                  = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // The magic happens in `unapply` of objects defined in mixed in traits
    override def transform(tree: Tree): Tree = tree match {
      case _ =>
        tree match {
          case tree @ ValOrDefDef(mods, name, tpt, rhs) =>
            if (mods.isImplicit && (tpt.tpe <:< TypeclassClass.tpe) && !mods.isSynthetic) {
              val tpe = tpt.tpe

              val isExplicitOrphan =
                if (mods.hasAccessorFlag) {
                  rhs match {
                    case field @ Select(q, name) =>
                      field.symbol.hasAnnotation(OrphanAttr)
                    case _ => false
                  }
                } else mods.hasAnnotationNamed(OrphanAttr.name)

              val enclosing: Symbol = tree.symbol.asTerm.owner

              if (enclosing.isAbstract) return super.transform(tree)
              if (mods.isParameter) return super.transform(tree)

              if (!enclosing.isModuleClass && !enclosing.isPackageObjectClass) {
                if (!isExplicitOrphan)
                  error(s"Orphan type instance: ${name.longString}")
              }

              val enclosingModule = enclosing.companionSymbol

              val nonOrphanLocations = tpe.typeConstructor.companion.termSymbol ::
                tpe.typeArgs.map(t => t.typeSymbol.companion)

              val isOrphan = !nonOrphanLocations.contains(enclosingModule)

              if (isOrphan) {
                if (!isExplicitOrphan)
                  error(s"Orphan type instance: ${name.longString}")
              }
            }
          case _ => ()
        }

        super.transform(tree)
    }
  }
}