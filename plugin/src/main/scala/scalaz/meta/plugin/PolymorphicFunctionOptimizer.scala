package scalaz.meta.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal

abstract class PolymorphicFunctionOptimizer
    extends PluginComponent
    with Transform
    with TypingTransformers
    with TreeDSL {
  val global: Global
  val scalazDefns: Definitions { val global: PolymorphicFunctionOptimizer.this.global.type }

  import global._, scalazDefns.{ global => _, _ }

  override val phaseName: String       = "scalaz-polyopt"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer =
    new MyTransformer(unit)

  def isSelect(tree: Tree): Boolean = tree match {
    case TypeApply(fun, args) => isSelect(fun)
    case Select(qualifier, _) => isSelect(qualifier)
    case Ident(a)             => true
    case This(_)              => true
    case Super(qual, _)       => isSelect(qual)
    case _                    => false
  }

  val symbolMethods = classOf[Symbol].getMethods.toList
    .filter(
      m =>
        m.getName.startsWith("is") &&
          m.getReturnType == classOf[Boolean] &&
          java.lang.reflect.Modifier.isPublic(m.getModifiers) &&
          !java.lang.reflect.Modifier.isStatic(m.getModifiers) &&
          m.getParameterCount == 0
    )
    .map { m =>
      m.setAccessible(true)
      m
    }
    .sortBy(_.getName)

  def showSymbol(s: Symbol): String =
    s.toString() + "[" + symbolMethods
      .filter(m => m.invoke(s).asInstanceOf[Boolean])
      .map(_.getName)
      .mkString(", ") + "]"

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def rewriteMethod(owner: Symbol, fun: DefDef): List[Tree] = {
      val name1 = freshTermName("local$")(currentFreshNameCreator)

      def substitute(tree: Tree): Tree =
        tree.substituteTypes(fun.tparams.map(_.symbol), fun.tparams.map(_ => definitions.AnyTpe))

      val tpt1 = substitute(fun.tpt)
      val rhs1 = substitute(fun.rhs)

      import scala.tools.nsc.symtab.Flags._

      val val1_symbol = owner
        .newTermSymbol(name1, fun.rhs.pos, FINAL | LOCAL | SYNTHETIC)
        .setInfoAndEnter(tpt1.tpe)

      val val1 = newValDef(val1_symbol, rhs1)(Modifiers(0), name1, tpt1)

      val fun1 = treeCopy.DefDef(
        fun,
        fun.mods,
        fun.name,
        fun.tparams,
        fun.vparamss,
        fun.tpt,
        TypeApply(Select(Select(This(owner), val1_symbol), "asInstanceOf"), List(fun.tpt))
      )

      List(localTyper.typedValDef(val1), localTyper.typedDefDef(fun1))
    }

    def createSuperCall(owner: Symbol, fun: MethodSymbol): List[Tree] = {
      val valName = freshTermName("local$")(currentFreshNameCreator)

      val defType = fun.typeSignature

      val valType =
        defType.finalResultType.substituteTypes(defType.typeParams, defType.typeParams.map(_ => definitions.AnyTpe))

      import scala.tools.nsc.symtab.Flags._

      val valSymbol = owner
        .newTermSymbol(valName, owner.pos, FINAL | LOCAL | SYNTHETIC)
        .setInfoAndEnter(valType)

      val t1 = Select(TypeApply(Select(Super(This(owner), fun.owner.asType.name), fun),
                                fun.typeParams.map(_ => TypeTree(definitions.AnyTpe))),
                      "asInstanceOf")

      val valRHSTree = TypeApply(t1, List(TypeTree(valType)))

      val valDefTree = newValDef(valSymbol, valRHSTree)()

      val defSymbol = owner
        .newMethodSymbol(fun.name.toTermName, owner.pos, FINAL | SYNTHETIC | OVERLOADED | METHOD)
        .setInfoAndEnter(fun.tpe)

      val defRHSTree =
        TypeApply(Select(Select(This(owner), valSymbol), "asInstanceOf"), List(TypeTree(defType.finalResultType)))

      val defTree = newDefDef(defSymbol, defRHSTree)()

      List(localTyper.typedValDef(valDefTree), localTyper.typedDefDef(defTree))
    }

    def isStaticScope(sym: Symbol): Boolean = {
      def go(syms: List[Symbol]): (Boolean, Boolean) = syms match {
        case s :: ss
            if s.isClass && !s.isModuleClass
              && !s.isPackageClass && !s.isPackageObjectClass =>
          val (staticScope, packageScope) = go(ss)
          if (s.isAnonymousClass) {
            (staticScope || packageScope, false)
          } else (false, false)

        case s :: ss if s.isPackageClass =>
          (false, true)

        case s :: ss if s.isModuleOrModuleClass =>
          val (staticScope, packageScope) = go(ss)
          (staticScope || packageScope, false)

        case s :: ss if s.isVal && !s.isParameter =>
          val (staticScope, packageScope) = go(ss)
          (staticScope || packageScope, false)

        case s :: ss if s.isVar && !s.isParameter =>
          (false, false)

        case s :: ss if s.isParamWithDefault =>
          (false, false)

        case s :: ss if s.isMethod =>
          (false, false)

        case s :: ss =>
          System.err.println(showSymbol(s))
          throw new Error()
      }

      go(sym :: currentOwner.ownerChain)._1
    }

    def processBody(owner: Symbol, tmpl: Template): Template = {
      val body = tmpl.body

      val tmplMethods = body.map(_.symbol).collect {
        case s
            if s.isMethod && !s.isConstructor &&
              s.asMethod.typeParams.nonEmpty && s.asMethod.paramLists.isEmpty =>
          s
      }

      val superMethods = tmpl.parents.flatMap { p =>
        val tmplOverrides = tmplMethods.map { m =>
          m.overriddenSymbol(p.symbol)
        }

        p.tpe.members.flatMap {
          case s
              if s.isMethod && !s.isFinal && !s.isConstructor &&
                s.asMethod.typeParams.nonEmpty && s.asMethod.paramLists.isEmpty &&
                !tmplOverrides.contains(s.asMethod) =>
            List(s.asMethod.substInfo(p.tpe.typeConstructor.typeParams, p.tpe.typeArgs.map(_.typeSymbol)))
          case _ => List()
        }
      }

      treeCopy.Template(
        tmpl,
        tmpl.parents,
        tmpl.self,
        body.flatMap {
          case fun @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
              if !mods.isSynthetic && !fun.symbol.isConstructor && !fun.symbol.isAccessor && !isSelect(rhs) =>
            if (tparams.nonEmpty && vparamss.isEmpty) {
              rewriteMethod(owner, fun)
            } else List(fun)
          case x => List(x)
        }
          ++ superMethods.flatMap { s =>
            createSuperCall(owner, s)
          }
      )
    }

    override def transform(tree: Tree): Tree =
      try {
        tree match {
          case cd @ ClassDef(_, _, _, tmpl @ Template(_, _, _)) if isStaticScope(cd.symbol) =>
            super.transform(treeCopy.ClassDef(tree, cd.mods, cd.name, cd.tparams, processBody(cd.symbol, tmpl)))
          case mod @ ModuleDef(_, _, tmpl @ Template(_, _, _)) if isStaticScope(mod.symbol) =>
            super.transform(treeCopy.ModuleDef(tree, mod.mods, mod.name, processBody(mod.symbol.moduleClass, tmpl)))
          case _ => super.transform(tree)
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          super.transform(tree)
      }
  }
}
