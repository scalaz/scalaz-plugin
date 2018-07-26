package scalaz.meta.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal
import scala.reflect.internal.Flags._

abstract class LazyValOptimizer
    extends PluginComponent
    with Transform
    with TypingTransformers
    with TreeDSL {
  val global: Global
  val scalazDefns: Definitions { val global: LazyValOptimizer.this.global.type }

  import global._
  import scalazDefns.{ global => _ }

  override val phaseName: String       = "scalaz-lazyvalopt"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    private def createMethodBody(owner: Symbol,
                                 flag: Symbol,
                                 flagValue: Int,
                                 holder: Symbol,
                                 body: Tree): Tree = {

      import CODE._

      val cond = {
        val bitwiseAnd = This(owner) DOT flag.name DOT TermName("$amp") APPLY List(
          Literal(Constant(flagValue))
        )
        bitwiseAnd DOT TermName("$bang$eq") APPLY List(Literal(Constant(0)))
      }

      // then part
      val thenp = BLOCK(
        (This(owner) DOT flag.name) === {
          This(owner) DOT flag.name DOT TermName("$bar") APPLY List(Literal(Constant(flagValue)))
        },
        (This(owner) DOT holder.name) === body
      )

      // else part empty and return holder value
      BLOCK(IF(cond) THEN thenp ELSE EmptyTree, This(owner) DOT holder.name)
    }

    def createField(owner: Symbol, name: TermName, tpe: Type, body: Tree): Tree = {
      import scala.reflect.internal.Flags._

      val variableSym = owner.newVariable(name, owner.pos.focus, newFlags = PrivateLocal | SYNTHETIC)
      variableSym.setInfoAndEnter(tpe)
      val variable = localTyper.typedPos(variableSym.pos.focus)(ValDef(variableSym, body))

      val getterSym =
        owner.newMethodSymbol(variableSym.name.getterName, owner.pos.focus, newFlags = ACCESSOR | SYNTHETIC)
      getterSym.setInfoAndEnter(NullaryMethodType(tpe))
      localTyper.typedPos(getterSym.pos.focus)(DefDef(getterSym, body))

      val setterSym =
        owner.newMethodSymbol(getterSym.name.setterName, owner.pos.focus, newFlags = ACCESSOR | SYNTHETIC)
      val setterParams = setterSym.newSyntheticValueParams(tpe :: Nil)
      setterSym.setInfoAndEnter(MethodType(setterParams, definitions.UnitTpe))
      localTyper.typedPos(setterSym.pos.focus)(DefDef(setterSym, EmptyTree))

      variable
    }

    private def createLazyvalOptimizer: (Symbol, ValDef, Tree) => List[Tree] = {
      var count: Int = -1
      (owner, lazyVal, flag) =>
        {
          // inc count
          count += 1

          // remove lazy val
          owner.info.decls.unlink(lazyVal.symbol)

          // create var value holder
          val holder =
            createField(owner,
                        TermName(lazyVal.name + "$value$" + count),
                        lazyVal.tpt.tpe,
                        EmptyTree)

          // create method with same name as lazy val
          val methodTerm = owner
            .newMethodSymbol(lazyVal.name, owner.pos.focus, SYNTHETIC)
            .setInfoAndEnter(NullaryMethodType(lazyVal.tpt.tpe))
          val method = localTyper.typedPos(methodTerm.pos.focus)(
            DefDef(methodTerm,
                   createMethodBody(owner, flag.symbol, 1 << count, holder.symbol, lazyVal.rhs))
          )

          List(holder, method)
        }
    }

    private def processBody(owner: Symbol, tmpl: Template): Template = {

      // create var flag for whole block
      val flagName = freshTermName("$lazyflag$")(currentFreshNameCreator)
      val flag     = createField(owner, flagName, definitions.IntTpe, Literal(Constant(0)))

      val optimizer = createLazyvalOptimizer

      treeCopy.Template(
        tmpl,
        tmpl.parents,
        tmpl.self,
        flag :: tmpl.body.flatMap {
          case lazyVal @ ValDef(mods, _, _, _) if mods.isLazy =>
            optimizer(owner, lazyVal, flag)
          case x =>
            List(x)
        }
      )
    }

    override def transform(tree: Tree): Tree =
      try {
        tree match {
          case cd @ ClassDef(_, _, _, tmpl @ Template(_, _, _)) =>
            super.transform(
              treeCopy.ClassDef(tree,
                                cd.mods,
                                cd.name,
                                cd.tparams,
                                processBody(cd.symbol.enclClass, tmpl))
            )
          case mod @ ModuleDef(_, _, tmpl @ Template(_, _, _)) =>
            super.transform(
              treeCopy.ModuleDef(tree,
                                 mod.mods,
                                 mod.name,
                                 processBody(mod.symbol.moduleClass, tmpl))
            )
          case _ => super.transform(tree)
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          super.transform(tree)
      }
  }

}
