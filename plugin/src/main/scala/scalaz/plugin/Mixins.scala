package scalaz.plugin

import scala.collection.mutable
import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.typechecker.AnalyzerPlugins
import scala.tools.nsc.{Global, Phase, plugins}
import scala.util.control.NonFatal

abstract class Mixins
    extends plugins.PluginComponent {
  val global: Global
  val scalazDefns: Definitions {val global: Mixins.this.global.type}

  implicit final class Ops[A](val self: A) {
    def opt[B](f: PartialFunction[A, B]): Option[B] = f.lift(self)
  }

  val mixinPlugin = new global.analyzer.MacroPlugin {
    override def isActive(): Boolean =
      global.phase.id < global.currentRun.picklerPhase.id

    val state: mutable.ListMap[String, StatePart] = mutable.ListMap.empty

    type TyParamSubstMap = Map[String, global.Type]
    type ValueParamSubstMap = Map[String, global.Symbol]

    case class StatePart(body: List[global.Tree], tyParamSyms: TyParamSubstMap, valueParamSyms: ValueParamSubstMap) {
      def substitute(newTyParamSyms: TyParamSubstMap, newValueParamSyms: ValueParamSubstMap): List[global.Tree] = {
        val (oldTys, newTys) = tyParamSyms.map { case (k, v) =>
          newTyParamSyms.get(k).map(ns => (v.typeSymbol, ns))
        }.toList.flatten.unzip
        val (oldVs, newVs) = valueParamSyms.map { case (k, v) =>
          newValueParamSyms.get(k).map(ns => (v, ns))
        }.toList.flatten.unzip
        body.map(
          _.substituteTypes(oldTys, newTys)
           .substituteSymbols(oldVs, newVs)
        )
      }
    }

    def instanceDecl: global.Tree => Option[global.DefDef] = _.opt {
      case defdef: global.DefDef
        if defdef.symbol.hasFlag(global.Flag.IMPLICIT) =>
        defdef
    }

    def extractInstanceParts(defDef: global.DefDef): Option[(global.ClassDef, global.Tree)] = defDef.rhs.opt {
      case global.Block(List(cld: global.ClassDef), instantiation) =>
        (cld, instantiation)
    }

    def extractInstanceTypeFromDeclType: global.Type => Option[(global.Type, TyParamSubstMap, ValueParamSubstMap)] = {
      case pt@global.PolyType(_, methTy: global.MethodType) =>
        val tyParamsMap = pt.params.map(p => (p.name.toString, p.toType)).toMap
        val valueParamsMap = methTy.params.map(p => (p.name.toString, p)).toMap
        Some((methTy.resultType, tyParamsMap, valueParamsMap))
      case methTy: global.MethodType =>
        val valueParamsMap = methTy.params.map(p => (p.name.toString, p)).toMap
        Some((methTy.resultType, Map.empty, valueParamsMap))
      case _ => None
    }

    def superClasses(tpe: global.Type): List[global.Type] = {
      tpe.baseTypeSeq.toList.filterNot { ty =>
        ty == tpe ||
          ty == global.definitions.AnyTpe ||
          ty == global.definitions.AnyRefTpe ||
          ty == global.definitions.ObjectTpe ||
          ty == global.definitions.SerializableTpe ||
          ty == scalazDefns.TypeclassType
      }
    }

    def listTraverseOption[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
      list.foldRight[Option[List[B]]](Some(Nil)) {
        case (a, bs) => f(a).flatMap(b => bs.map(b :: _))
      }

    def removeInit(stats: List[global.Tree]): List[global.Tree] = {
      stats.filter {
        case dd: global.DefDef => dd.name.toString != "<init>"
        case _ => true
      }
    }

    def expandTree(typer: global.analyzer.Typer, tr: global.Tree): global.Tree = {
      val newTree = for {
        defdef <- instanceDecl(tr)
        (anonClass, newInstance) <- extractInstanceParts(defdef)
        (instanceTy, tySubstMap, valSubstMap) <- extractInstanceTypeFromDeclType(defdef.symbol.info)
        () <- instanceTy match {
          case _: global.NoType.type => None
          case _ => Some(())
        }
        scs = superClasses(instanceTy)
        () <- if (state.contains(instanceTy.typeSymbol.name.toString)) {
          None
        } else {
          state += (instanceTy.typeSymbol.name.toString -> StatePart(removeInit(anonClass.impl.body), tySubstMap, valSubstMap))
          Some(())
        }
        extraCode = listTraverseOption(scs)(t => state.get(t.typeSymbol.name.toString)).map {
          _.flatMap(_.substitute(tySubstMap, valSubstMap))
        }
        newTree = extraCode match {
          case Some(code) if code.nonEmpty =>
            defdef.copy(rhs = global.Block(
              List(
                anonClass.copy(impl = anonClass.impl.copy(body = anonClass.impl.body ++ code))
              ), newInstance
            )).duplicate.setSymbol(global.NoSymbol)
          case _ =>
            tr
        }
      } yield newTree
      newTree match {
        case None => tr
        case Some(nt) =>
          typer.context.owner.info.decls.unlink(tr.symbol)
          typer.namer.enterSym(nt)
          nt
      }
    }

    override def pluginsEnterStats(typer: global.analyzer.Typer, stats: List[global.Tree]): List[global.Tree] = {
      if (typer.context.owner.hasAnnotation(scalazDefns.InstancesAttr)) {
        stats.map(expandTree(typer, _))
      } else {
        stats
      }
    }

  }

  global.analyzer.addMacroPlugin(mixinPlugin)

  import global._, scalazDefns.{global => _, _}

  override val phaseName: String = "scalaz-mixins"
  override val runsAfter: List[String] = "namer" :: Nil
  override val runsBefore: List[String] = Nil

  import definitions.AnyRefTpe

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = ()
  }
}
