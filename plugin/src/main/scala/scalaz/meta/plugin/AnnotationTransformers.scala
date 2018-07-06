package scalaz.meta.plugin

import scala.reflect.internal.Mode._
import scala.tools.nsc.Global

trait AnnotationTransformers {
  val global: Global
  import global._

  abstract class AnnotatedClassTransformer(predicate: Symbol => Boolean)
      extends analyzer.AnalyzerPlugin
      with analyzer.MacroPlugin {
    import analyzer.{ global => _, _ }

    case object HasAnnotatedClass

    override def isActive(): Boolean =
      global.phase.id < global.currentRun.picklerPhase.id

    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
      val ctx = typer.context
      defTree match {
        case classImpl: Template if ctx.owner.isClass =>
          for (member <- classImpl.body) {
            val annPairs: List[Tree] = member match {
              case ClassDef(mods, _, _, _) =>
                mods.annotations
              case _ => Nil
            }
            println(s"\tannPairs: $annPairs")
            val maybe = annPairs.collectFirst {
              case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if {
                    predicate(
                      newTyper(ctx.outer)
                        .typed(tpt.duplicate, TYPEmode, WildcardType)
                        .symbol
                    )
                  } =>
                args.headOption
            }
            println(s"\tmaybe: $maybe")
            maybe.foreach { providedName =>
              member.updateAttachment(HasAnnotatedClass)
            }
          }
        case _ =>
      }
      tpe
    }

    def transformStats(localTyper: Typer, stats: List[Tree]): (Boolean, List[Tree]) = {
      val annottees = stats.flatMap {
        case c: ClassDef if c.hasAttachment[HasAnnotatedClass.type] =>
          Some(c)
        case _ => None
      }.map { c =>
        val companion = analyzer.companionSymbolOf(c.symbol, localTyper.context)
        val m = stats.collectFirst {
          case m: ModuleDef if m.symbol == companion => m
        }
        (c, m)
      }

      if (annottees.isEmpty) (false, stats)
      else {
        println(annottees, stats)

        val annotteeMap     = annottees.map { case t @ (c, m) => (c.symbol, t) }.toMap
        val annotteeModules = annottees.flatMap { case (c, m) => m.map(_.symbol) }.toSet

        (true, stats.flatMap { t =>
          annotteeMap.get(t.symbol) match {
            case None =>
              if (annotteeModules.contains(t.symbol)) Nil else List(t)
            case Some((c, m)) => transformAnnotated(c, m)
          }
        })
      }
    }

    override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
      println("pluginsEnterStats")

      val (run, newStats) = transformStats(typer, stats)
      newStats
    }

    def transformAnnotated(clsDef: ClassDef, modDef: Option[ModuleDef]): List[Tree]
  }
}
