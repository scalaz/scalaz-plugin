package scalaz.plugin

import miniz._

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.tools.nsc.{ plugins, Global }
import scala.util.control.NonFatal

abstract class OrphanChecker
    extends plugins.PluginComponent
    with Transform
    with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: OrphanChecker.this.global.type }

  import global._, scalazDefns.{ global => _, _ }

  override val phaseName: String       = "scalaz-orphans"
  override val runsAfter: List[String] = "typer" :: Nil

  import definitions.AnyRefTpe

  def newTransformer(unit: CompilationUnit): Transformer =
    new MyTransformer(unit)

  final case class SimpleTypeclassType(constructor: Type,
                                       args: List[Type],
                                       orphanAnnotation: Boolean)
  object SimpleTypeclassType {
    def check(tpe: Type): Option[SimpleTypeclassType] = {
      val dealiased = tpe.dealias
      dealiased match {
        case TypeRef(pre, sym, args) if dealiased <:< TypeclassClass.tpe =>
          Some(SimpleTypeclassType(dealiased.typeConstructor, args, false))
        case AnnotatedType(annotations, underlying) =>
          SimpleTypeclassType.check(underlying).map {
            _.copy(orphanAnnotation = annotations.exists(a => a.matches(OrphanAttr)))
          }
        case _ => None
      }
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
          case TypeRef(pre, sym, args) if sym.isAnonymousClass && sym.hasCompleteInfo =>
            nonOrphanLocations(
              RefinedType(sym.asClass.info.parents.filterNot(_ =:= AnyRefTpe), EmptyScope)
            )
          case RefinedType(parents, decls) =>
            parents.flatMap(nonOrphanLocations)
          case TypeRef(pre, sym, args) =>
            val companions =
              dealiased.typeConstructor.typeSymbol ::
                dealiased.typeArgs.map(t => t.typeSymbol)
            companions
              .flatMap(s => s :: s.companionModule.moduleClass :: s.companionModule :: Nil)
              .filter(s => s != NoSymbol)
          case AnnotatedType(annotations, underlying) =>
            nonOrphanLocations(underlying)
          case _ =>
            globalError(s"Unknown typeclass definition format: ${showRaw(tpe)}"); Nil
        }
      }

    def propositionTree(tree: Tree): Boolean = tree match {
      case tree @ Select(_, _) if tree.tpe <:< TypeclassClass.tpe        => true
      case Apply(fun, args) if args.forall(_.tpe <:< TypeclassClass.tpe) => true
      case _                                                             => false
    }

    def validateClassDeclaration(pos: Position, sym: Symbol, parents: List[Tree]): Unit =
      parents
        .map(_.tpe)
        .flatMap(
          t =>
            SimpleTypeclassType.check(t) match {
              case Some(typeclassType) => List(Right(typeclassType))
              case None =>
                if (t =:= AnyRefTpe) Nil
                else List(Left(t))
          }
        )
        .separate match {
        case Nil /\ Nil =>
          globalError(
            pos,
            "Internal error: typeclass instance declaration has no parents.\n" +
              "This should be impossible. Please file an issue at https://github.com/scalaz/scalaz-plugin/issues"
          )

        case (l @ _ :: _) /\ _ =>
          globalError(pos, s"""Typeclass instance declaration has some unrecognized parents:
                              |${l.mkString(", ")}
             """.stripMargin)

        case Nil /\ typeclassParents =>
          val enclosing: Set[Symbol] = Set(currentOwner.ownerChain: _*)

          val symbolSets = typeclassParents.flatMap {
            case SimpleTypeclassType(tc, args, orphan) =>
              val ss = tc.typeConstructor.typeSymbol :: args.map(_.typeSymbol)
              if (orphan) None else Some(Set(ss: _*))
          }

          if (symbolSets.nonEmpty) {
            val locations = symbolSets
              .reduce[Set[Symbol]] { case (a, b) => a.intersect(b) }
              .flatMap(s => s :: s.companionModule.moduleClass :: s.companionModule :: Nil)
              .filter(s => s != NoSymbol)

            if (locations.intersect(enclosing).isEmpty) {
              val orphansAllowed =
                analyzer.inferImplicitByType(EnableOrphansFlag.tpe, localTyper.context)

              if (orphansAllowed.isFailure) {
                globalError(pos, "Orphan instance.")
              }
            }
          }
      }

    def checkTreeNodeForOrphans(tree: Tree): Unit = tree match {
      case Apply(fun, args) =>
        val tpe = tree.tpe.dealias

        if (!(tpe <:< TypeclassType) || tpe <:< definitions.NothingTpe) return

        val enclosing: Set[Symbol] =
          Set(currentOwner.ownerChain: _*)
        val locations: Set[Symbol] =
          Set(nonOrphanLocations(tpe): _*)

        if (enclosing.intersect(locations).isEmpty) {
          val orphansAllowed =
            analyzer.inferImplicitByType(EnableOrphansFlag.tpe, localTyper.context)
          if (!propositionTree(tree) && orphansAllowed.isFailure) {
            globalError(tree.pos, "Non-propositional expression involving typeclasses.")
          }
        }

      case cd @ ClassDef(mods, _, _, Template(parents, _, _)) if cd.symbol.tpe <:< TypeclassType =>
        val sym = cd.symbol
        if (sym.isClass && !sym.isAbstract) {
          validateClassDeclaration(cd.pos, sym, parents)
        }

      case md @ ModuleDef(_, _, Template(parents, _, _)) if md.symbol.tpe <:< TypeclassType =>
        validateClassDeclaration(md.pos, md.symbol, parents)

      case _ => ()
    }

    override def transform(tree: Tree): Tree = {
      try {
        checkTreeNodeForOrphans(tree)
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          throw e
      }
      super.transform(tree)
    }
  }
}
