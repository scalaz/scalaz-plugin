package scalaz.meta.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal

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

    private def createMethodBody(owner: Symbol, flag: Symbol, holder: Symbol, body: Tree): Tree = {

      import CODE._

      val cond = {
        val bitwiseAnd = This(owner) DOT flag.name DOT TermName("$amp") APPLY List(
          Literal(Constant(1))
        )
        bitwiseAnd DOT TermName("$bang$eq") APPLY List(Literal(Constant(0)))
      }

      // then part
      val thenp = BLOCK(
        (This(owner) DOT flag.name) === {
          This(owner) DOT flag.name DOT TermName("$bar") APPLY List(Literal(Constant(1)))
        },
        (This(owner) DOT holder.name) === body
      )

      // else part empty and return holder value
      BLOCK(IF(cond) THEN thenp ELSE EmptyTree, This(owner) DOT holder.name)
    }

    def createField(owner: Symbol, name: TermName, tpe: Type, body: Tree): Tree = {
      import scala.reflect.internal.Flags._

      val variableSym = owner.newVariable(name, owner.pos.focus, newFlags = PrivateLocal)
      variableSym.setInfoAndEnter(tpe)
      val variable = localTyper.typedPos(variableSym.pos.focus)(ValDef(variableSym, body))

      val getterSym =
        owner.newMethodSymbol(variableSym.name.getterName, owner.pos.focus, newFlags = ACCESSOR)
      getterSym.setInfoAndEnter(NullaryMethodType(tpe))
      val getter = localTyper.typedPos(getterSym.pos.focus)(DefDef(getterSym, body))

      val setterSym =
        owner.newMethodSymbol(getterSym.name.setterName, owner.pos.focus, newFlags = ACCESSOR)
      val setterParams = setterSym.newSyntheticValueParams(tpe :: Nil)
      setterSym.setInfoAndEnter(MethodType(setterParams, definitions.UnitTpe))
      val setter = localTyper.typedPos(setterSym.pos.focus)(DefDef(setterSym, EmptyTree))

      variable
    }

    private def optimizeLazyVal(owner: Symbol, lazyVal: ValDef): List[Tree] = {
      import scala.reflect.internal.Flags._

      // remove lazy val
      owner.info.decls.unlink(lazyVal.symbol)

      // create var flag
      val flagName = freshTermName("$lazyflag$")(currentFreshNameCreator)
      val flag =
        createField(owner, flagName, definitions.IntTpe, Literal(Constant(0)))

      // create var value holder
      val holder =
        createField(owner, TermName(lazyVal.name + "$value$"), lazyVal.tpt.tpe, EmptyTree)

      // create method with same name as lazy val
      val methodTerm = owner
        .newMethodSymbol(lazyVal.name, owner.pos.focus, FINAL | SYNTHETIC | METHOD)
        .setInfoAndEnter(NullaryMethodType(lazyVal.tpt.tpe))
      val method = localTyper.typedPos(methodTerm.pos.focus)(
        DefDef(methodTerm, createMethodBody(owner, flag.symbol, holder.symbol, lazyVal.rhs))
      )

      List(flag, holder, method)
    }

    private def processBody(owner: Symbol, tmpl: Template): Template =
      treeCopy.Template(
        tmpl,
        tmpl.parents,
        tmpl.self,
        tmpl.body.flatMap { // TODO: this will create flag for each lazy val; will combined them all in single int
          case lazyVal @ ValDef(mods, _, _, _) if mods.isLazy =>
            optimizeLazyVal(owner, lazyVal)
          case x =>
            List(x)
        }
      )

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
