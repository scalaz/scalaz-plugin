package scalaz.meta.plugin

import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.tools.nsc.{ plugins, Global }

abstract class OrphanChecker extends plugins.PluginComponent with Transform with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: OrphanChecker.this.global.type }

  import global._, scalazDefns.{ global => _, _ }

  override val phaseName: String       = "scalaz-orphans"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  def oldCode(tree: Tree, mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): Unit =
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

//      if (enclosing.isAbstract) return super.transform(tree)
//      if (mods.isParameter) return super.transform(tree)

      if (!enclosing.isModuleClass && !enclosing.isPackageObjectClass) {
        if (!isExplicitOrphan)
          error(s"Orphan type instance: ${name.longString}")
      }

      val enclosingModule = enclosing.companionSymbol

      val nonOrphanLocations = tpe.typeConstructor.typeSymbol.companion ::
        tpe.typeArgs.map(t => t.typeSymbol.companion)

      val isOrphan = !nonOrphanLocations.contains(enclosingModule)

      if (isOrphan) {
        if (!isExplicitOrphan)
          error(s"Orphan type instance: ${name.longString}")
      }
    }

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // FIXME: We don't really transform the tree, merely traverse.
    // Might need the typer for implicit resolution.

    def nonOrphanLocations(tpe: Type): List[Symbol] =
      if (!(tpe <:< TypeclassClass.tpe)) Nil
      else {
        val dealiased = tpe.dealias
        dealiased match {
          case RefinedType(parents, decls) =>
            parents.flatMap(t => nonOrphanLocations(t))
          case TypeRef(pre, sym, args) =>
            val companions =
              dealiased.typeConstructor.typeSymbol ::
                dealiased.typeArgs.map(t => t.typeSymbol)
            companions
              .flatMap(s => s :: s.companionModule.moduleClass :: s.companionModule :: Nil)
              .filter(s => s != NoSymbol)
          case _ => error("Unknown typeclass definition format"); Nil
        }
      }

    def propositionTree(tree: Tree): Boolean = tree match {
      case tree @ Select(_, _) if tree.tpe <:< TypeclassClass.tpe             => true
      case Apply(fun, args) if args.forall(a => a.tpe <:< TypeclassClass.tpe) => true
      case _                                                                  => false
    }

    def accessorToExplicitOrphan(tree: Tree, mods: Modifiers, rhs: Tree): Boolean =
      if (mods.hasAccessorFlag) {
        rhs match {
          case field @ Select(_, _) => field.symbol.hasAnnotation(OrphanAttr)
          case _                    => false
        }
      } else false

    override def transform(tree: Tree): Tree = {
      tree match {
        case ValOrDefDef(mods, name, tpt, rhs) if tpt.tpe <:< TypeclassClass.tpe && !rhs.isEmpty =>
          // A typeclass val or def.

          val tpe = tpt.tpe

          val isExplicitOrphan = rhs.tpe.hasAnnotation(OrphanAttr)

          val enclosing: Set[Symbol] =
            Set(tree.symbol.asTerm.ownerChain: _*)
          val locations: Set[Symbol] =
            Set(nonOrphanLocations(tpe): _*)

          if (enclosing.intersect(locations).isEmpty) {
            if (!propositionTree(rhs) && !isExplicitOrphan) {
              globalError(rhs.pos, "Orphan instance.")
              // $enclosing
              //                   | ${enclosing.map(_.getClass)}
              //                   | $locations
              //                   | ${locations.map(_.getClass)}
              //                   | $rhs
              //                   | ${showRaw(rhs)}
            }
          }

        case _ => ()
      }
      super.transform(tree)
    }
  }
}
