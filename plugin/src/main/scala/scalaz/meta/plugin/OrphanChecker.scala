package scalaz.meta.plugin

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.tools.nsc.{ plugins, Global }
import scala.util.control.NonFatal

abstract class OrphanChecker extends plugins.PluginComponent with Transform with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: OrphanChecker.this.global.type }

  import global._, scalazDefns.{ global => _, _ }

  override val phaseName: String       = "scalaz-orphans"
  override val runsAfter: List[String] = "typer" :: Nil

  import definitions.AnyRefTpe

  def newTransformer(unit: CompilationUnit): Transformer =
    new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // FIXME: We don't really transform the tree, merely traverse.
    // Might need the typer for implicit resolution.

    def nonOrphanLocations(tpe: Type): List[Symbol] =
      if (!(tpe <:< TypeclassClass.tpe)) Nil
      else {
        val dealiased = tpe.dealias
        dealiased match {
          case TypeRef(pre, sym, args) if sym.isAnonymousClass && sym.hasCompleteInfo =>
            nonOrphanLocations(RefinedType(sym.asClass.info.parents.filterNot(_ =:= AnyRefTpe), EmptyScope))
          case RefinedType(parents, decls) =>
            parents.flatMap(t => nonOrphanLocations(t))
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
      case tree @ Select(_, _) if tree.tpe <:< TypeclassClass.tpe             => true
      case Apply(fun, args) if args.forall(a => a.tpe <:< TypeclassClass.tpe) => true
      case _                                                                  => false
    }

    object SimpleTypeclassType {
      def unapply(tpe: Type): Option[(Type, List[Type], Boolean)] = {
        val dealiased = tpe.dealias
        dealiased match {
          case TypeRef(pre, sym, args) if dealiased <:< TypeclassClass.tpe =>
            Some((dealiased.typeConstructor, args, false))
          case AnnotatedType(annotations, underlying) =>
            SimpleTypeclassType.unapply(underlying).map {
              _.copy(_3 = annotations.exists(a => a.matches(OrphanAttr)))
            }
          case _ => None
        }
      }
    }

    def isExplicitOrphanNew(tree: Tree): Boolean = {
      val treeAnnotation = tree.tpe.hasAnnotation(OrphanAttr)
      val ownerAnnotation = if (currentOwner.isTerm) {
        currentOwner.asTerm.hasAnnotation(OrphanAttr)
      } else false

      inform(tree.pos, s"""isExplicitOrphanNew
                          | $treeAnnotation
                          | $ownerAnnotation
         """.stripMargin)
      treeAnnotation || ownerAnnotation
    }

    def validClassDelc(pos: Position, sym: Symbol, parents: List[Tree]): Unit = {
      // first base class is `sym`
      val baseClasses = sym.baseClasses.tail.filter { c =>
        c != definitions.ObjectClass &&
        c != definitions.AnyRefClass &&
        c != definitions.AnyClass &&
        c != definitions.AnyValClass &&
        c != definitions.SerializableClass &&
        c != TypeclassClass
      }

      if (!baseClasses.forall(_.tpe <:< TypeclassType)) {
        error.InvalidTypeclassInstanceDeclaration(pos)
        return
      }

      val enclosing: Set[Symbol] =
        Set(currentOwner.ownerChain: _*)

      val ps = parents.map(_.tpe).collect {
        case SimpleTypeclassType(pair) => pair
      }

      if (ps.isEmpty) {
        error.InvalidTypeclassInstanceDeclaration(pos)
        return
      }

      val symbolSets = ps.flatMap {
        case (tc, args, orphan) =>
          val ss = tc.typeConstructor.typeSymbol :: args.map(_.typeSymbol)
          if (orphan) None
          else Some(Set(ss: _*))
      }

      if (symbolSets.nonEmpty) {
        val locations = symbolSets
          .reduce[Set[Symbol]] { case (a, b) => a.intersect(b) }
          .flatMap(s => s :: s.companionModule.moduleClass :: s.companionModule :: Nil)
          .filter(s => s != NoSymbol)

        if (locations.intersect(enclosing).isEmpty) {
          val orphansAllowed =
            analyzer.inferImplicitByType(EnableOrphansFlag.tpe, localTyper.context)

          if (orphansAllowed.isFailure)
            globalError(pos, "Orphan instance.")
        }
      }
    }

    def check(tree: Tree): Unit = tree match {
      case Apply(fun, args) =>
        val tpe = tree.tpe.dealias

        if (!(tpe <:< TypeclassType)) return

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
          validClassDelc(cd.pos, sym, parents)
        }

      case md @ ModuleDef(_, _, Template(parents, _, _)) if md.symbol.tpe <:< TypeclassType =>
        validClassDelc(md.pos, md.symbol, parents)

      case _ => ()
    }

    override def transform(tree: Tree): Tree = {
      try {
        check(tree)
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          throw e
      }
      super.transform(tree)
    }
  }

  object error {

    def InvalidTypeclassInstanceDeclaration(pos: Position): Unit =
      globalError(pos, "Invalid typeclass instance declaration.")
  }
}
