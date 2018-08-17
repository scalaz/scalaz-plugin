package scalaz.meta.plugin

import java.io.{ PrintWriter, StringWriter }

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal

abstract class PolymorphicFunctionOptimizer
    extends PluginComponent
    with Transform
    with TypingTransformers
    with TreeDSL
    with Utils {
  val global: Global
  val scalazDefns: Definitions { val global: PolymorphicFunctionOptimizer.this.global.type }

  import global._
  import scalazDefns.{ global => _ }

  override val phaseName: String       = "scalaz-polyopt"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer =
    new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def rewriteMethod(owner: Symbol, fun: DefDef): List[Tree] = {
      devWarning(s"rewriting ${fun.symbol} in $owner")

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
      devWarning(s"rewriting $fun in $owner as super call")

      val valName = freshTermName("local$")(currentFreshNameCreator)

      val defType = fun.typeSignature

      val valType =
        defType.finalResultType
          .substituteTypes(defType.typeParams, defType.typeParams.map(_ => definitions.AnyTpe))

      import scala.tools.nsc.symtab.Flags._

      val valSymbol = owner
        .newTermSymbol(valName, owner.pos, FINAL | LOCAL | SYNTHETIC)
        .setInfoAndEnter(valType)

      val t1 = Select(
        TypeApply(
          Select(Super(This(owner), ""), fun),
          fun.typeParams.map(_ => TypeTree(definitions.AnyTpe))
        ),
        "asInstanceOf"
      )

      val valRHSTree = TypeApply(t1, List(TypeTree(valType)))

      val valDefTree = newValDef(valSymbol, valRHSTree)()

      val defSymbol = owner
        .newMethodSymbol(fun.name.toTermName, owner.pos, FINAL | SYNTHETIC | OVERLOADED | METHOD)
        .setInfoAndEnter(fun.tpe)

      val defRHSTree =
        TypeApply(
          Select(Select(This(owner), valSymbol), "asInstanceOf"),
          List(TypeTree(defType.finalResultType))
        )

      val defTree = newDefDef(defSymbol, defRHSTree)()

      List(localTyper.typedValDef(valDefTree), localTyper.typedDefDef(defTree))
    }

    def processBody(owner: Symbol, tmpl: Template): Template = {
      val body = tmpl.body

      // First we find all non-constructor methods that don't have
      // any (non-type) parameters.
      val methods = collectParameterlessPolymorphicMethods(tmpl)

      val superMethods = owner.tpe.members
        .filter(_.isMethod)
        .map(_.asMethod)
        .filter(isParameterlessPolymorphicMethod)
        .filter(
          s =>
            s.name.toString != "asInstanceOf"
              && s.name.toString != "isInstanceOf"
        )
        .filter(s => !methods.contains(s))
        .filter(s => !s.isEffectivelyFinal)

      superMethods.foreach { s =>
        s.owner = owner
      }

      treeCopy.Template(
        tmpl,
        tmpl.parents,
        tmpl.self,
        body.flatMap {
          case fun @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
              if !mods.isSynthetic && !fun.symbol.isConstructor && !fun.symbol.isAccessor
                && !isSelect(rhs) =>
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
          case cd @ ClassDef(_, _, _, tmpl @ Template(_, _, _))
              if isStaticScope(cd.symbol, currentOwner) =>
            super.transform(
              treeCopy.ClassDef(tree, cd.mods, cd.name, cd.tparams, processBody(cd.symbol, tmpl))
            )
          case mod @ ModuleDef(_, _, tmpl @ Template(_, _, _))
              if isStaticScope(mod.symbol, currentOwner) =>
            super.transform(
              treeCopy
                .ModuleDef(tree, mod.mods, mod.name, processBody(mod.symbol.moduleClass, tmpl))
            )
          case _ => super.transform(tree)
        }
      } catch {
        case NonFatal(e) =>
          val sw = new StringWriter()
          val pw = new PrintWriter(sw)
          e.printStackTrace(pw)
          globalError(tree.pos, sw.toString)

          super.transform(tree)
      }
  }
}
