package scalaz.plugin

import scala.collection.mutable
import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.typechecker.AnalyzerPlugins
import scala.tools.nsc.{Global, Phase, plugins}
import scala.util.control.NonFatal

import miniz._

abstract class Mixins
    extends plugins.PluginComponent
    with Utils {
  val global: Global
  val scalazDefns: Definitions {val global: Mixins.this.global.type}

  val mixinPlugin = new global.analyzer.MacroPlugin {
    override def isActive(): Boolean =
      global.phase.id < global.currentRun.picklerPhase.id

    type TyParamSubstMap = Map[String, global.Symbol]
    type ValueParamSubstMap = Map[String, global.Symbol]

    case class StatePart(body: List[global.Tree], tyParamSyms: TyParamSubstMap, valueParamSyms: ValueParamSubstMap, oldAnonClass: global.Symbol) {
      def substitute(newTyParamSyms: TyParamSubstMap, newValueParamSyms: ValueParamSubstMap, newAnonClass: global.Symbol): List[global.Tree] = {
        val (oldTys, newTys) = tyParamSyms.map { case (k, v) =>
          newTyParamSyms.get(k).map(ns => (v.newTypeSkolem, ns.newTypeSkolem))
        }.toList.flatten.unzip
        val (oldVs, newVs) = valueParamSyms.map { case (k, v) =>
          newValueParamSyms.get(k).map(ns => (v, ns))
        }.toList.flatten.unzip
        body.map { b =>
          // substituteSymbols mutates the tree in place
          b.duplicate
            .substituteSymbols(
              oldVs ++ oldTys ++ List(oldAnonClass),
              newVs ++ newTys ++ List(newAnonClass)
            )
            .changeOwner(oldAnonClass -> newAnonClass)
        }
      }
    }

    def isInstanceDecl(defn: global.ValOrDefDef): Boolean = {
      // the last case is required to find backing `val`s
      val realResultType = defn.symbol.info match {
        case global.PolyType(_, methTy: global.MethodType) => methTy.resultType
        case methTy: global.MethodType => methTy.resultType
        case ty => ty
      }
      val isTypeClassType = realResultType.baseTypeSeq.toList.contains[global.Type](scalazDefns.TypeclassType)
      val isUnmixin = defn.symbol.hasAnnotation(scalazDefns.UnmixinAttr)
      isTypeClassType && !isUnmixin
    }

    def instanceDecl: global.Tree => Option[global.ValOrDefDef] = _.opt {
      case defn: global.ValOrDefDef if isInstanceDecl(defn) =>
        defn
    }

    def extractInstanceParts(defn: global.ValOrDefDef): Option[(global.ClassDef, global.Tree)] = defn.rhs.opt {
      case global.Block(List(cld: global.ClassDef), instantiation) =>
        (cld, instantiation)
    }

    def extractInstanceTypeFromDeclType: global.Type => (global.Type, TyParamSubstMap, ValueParamSubstMap) = {
      case pt@global.PolyType(_, methTy: global.MethodType) =>
        val tyParamsMap = pt.params.map(p => (p.name.toString, p)).toMap
        val valueParamsMap = methTy.params.map(p => (p.name.toString, p)).toMap
        (methTy.resultType, tyParamsMap, valueParamsMap)
      case methTy: global.MethodType =>
        val valueParamsMap = methTy.params.map(p => (p.name.toString, p)).toMap
        (methTy.resultType, Map.empty, valueParamsMap)
      case nme: global.NullaryMethodType =>
        (nme.resultType, Map.empty, Map.empty)
      case ty =>
        (ty, Map.empty, Map.empty)
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
        case dd: global.ValOrDefDef => dd.name.toString != "<init>"
        case _ => true
      }
    }

    def expandTree(state: mutable.ListMap[String, StatePart], typer: global.analyzer.Typer, tr: global.Tree): Either[LocatedError, global.Tree] = {
      instanceDecl(tr) match {
        case None =>
          Right(tr)
        case Some(defn) =>
          for {
            t <- extractInstanceParts(defn).orError(
              "Type class instance definition is only allowed to contain `new InstanceType {<body>}`".errorAt(defn.pos)
            )
            (anonClass, newInstance) = t
            // only safe once we know that `defn` is really a `DefDef` or `ValDef`
            (instanceTy, tySubstMap, valSubstMap) = extractInstanceTypeFromDeclType(defn.symbol.info)
            _ <- instanceTy match {
              case _: global.NoType.type => Left("Type class instance definition wasn't type checked? Report this".errorAt(defn.pos))
              case _ => Right(())
            }
            scs = superClasses(instanceTy)
            _ <- if (state.contains(instanceTy.typeSymbol.name.toString)) {
              Left("Only one instance in an @instances object is allowed per type class.".errorAt(defn.pos))
            } else {
              state += (instanceTy.typeSymbol.name.toString -> StatePart(removeInit(anonClass.impl.body), tySubstMap, valSubstMap, anonClass.symbol))
              Right(())
            }
            extraCode = listTraverseOption(scs)(t => state.get(t.typeSymbol.name.toString)).map {
              _.flatMap(_.substitute(tySubstMap, valSubstMap, anonClass.symbol))
            }
            newTree <- extraCode match {
              case Some(Nil) => Right(tr)
              case Some(code) =>
                val newBody = global.Block(
                    List(
                      global.deriveClassDef(
                        anonClass
                      )(_.copy(body = anonClass.impl.body ++ code))
                    ), newInstance
                  ).duplicate
                if (defn.isInstanceOf[global.DefDef])
                  Right(global.deriveDefDef(defn)(_ =>
                    newBody
                  ))
                else
                  Right(global.deriveValDef(defn)(_ =>
                    newBody
                  ))
              case None =>
                val missingClasses = scs.map(_.typeSymbol.name.toString).toSet -- state.keySet
                Left(s"A type class instance is missing instances of its parent classes: ${missingClasses.mkString(",")}".errorAt(defn.pos))
            }
          } yield {
            newTree
          }
      }
    }

    override def pluginsEnterStats(typer: global.analyzer.Typer, stats: List[global.Tree]): List[global.Tree] = {
      if (typer.context.owner.hasAnnotation(scalazDefns.InstancesAttr)) {
        val state: mutable.ListMap[String, StatePart] = mutable.ListMap.empty
        stats.map(o => expandTree(state, typer, o).map(n => (o, n))).uncozip.fold({ es =>
          es.foreach {
            case LocatedError(pos, msg) =>
              global.reporter.error(pos, msg)
          }
          stats
        }, { ts => ts.foreach { case (o, n) =>
          if (o ne n) {
            typer.context.owner.info.decls.unlink(o.symbol)
            typer.namer.enterSym(n)
          }
          n
        }; ts.map(_._2) })
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

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = ()
  }
}
