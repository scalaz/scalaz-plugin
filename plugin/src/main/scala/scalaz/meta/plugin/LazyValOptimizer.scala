package scalaz.meta.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.util.control.NonFatal

abstract class LazyValOptimizer extends PluginComponent with Transform with TypingTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: LazyValOptimizer.this.global.type }

  import global._
  import scalazDefns.{ global => _ }

  override val phaseName: String       = "scalaz-lazyvalopt"
  override val runsAfter: List[String] = "typer" :: Nil

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    private def createMethodBody(owner: Symbol, flag: Symbol, holder: Symbol, body: Tree): Tree = {
      val cond: Tree = {
        Apply(
          Select(
            Apply(Select(Select(This(owner), flag.name), TermName("$amp")),
                  List(Literal(Constant(1)))),
            TermName("$bang$eq")
          ),
          List(Literal(Constant(0)))
        )
      }

      val thenp: Tree = Block(
        List(
          Assign(
            Select(This(owner), flag.name),
            Apply(Select(Select(This(owner), flag.name), TermName("$bar")),
                  List(Literal(Constant(1))))
          )
        ),
        Assign(Select(This(owner), holder.name), body)
      )

      val elsep: Tree = Literal(Constant(()))

      val ret: Tree = Select(This(owner), holder.name)

      Block(List(If(cond, thenp, elsep)), ret)
    }

    private def createGetterSetter(owner: Symbol,
                                   name: TermName,
                                   tpe: Type,
                                   value: Tree): List[Tree] = {
      import scala.reflect.internal.Flags._

      val getterSym =
        owner.newMethodSymbol(name, owner.pos.focus, newFlags = ACCESSOR | MUTABLE | PRIVATE)
      getterSym.setInfoAndEnter(NullaryMethodType(tpe))
      val getter = localTyper.typedPos(getterSym.pos.focus)(ValDef(getterSym, value))

      val setterSym = owner.newMethodSymbol(getterSym.name.setterName,
                                            owner.pos.focus,
                                            newFlags = ACCESSOR | MUTABLE | PRIVATE)
      val setterParams = setterSym.newSyntheticValueParams(tpe :: Nil)
      setterSym.setInfoAndEnter(MethodType(setterParams, definitions.UnitTpe))
      val setter = localTyper.typedPos(setterSym.pos.focus)(DefDef(setterSym, EmptyTree))

      getter :: setter :: Nil
    }

    private def print[A](v: A): Unit = {
      println
      println(v)
      println
      println(showRaw(v))
      println
    }

    private def rewriteLazyVal(owner: Symbol, lazyVal: ValDef): List[Tree] = {
      import scala.reflect.internal.Flags._

      // create var flag
      val flagName = freshTermName("$lazyflag$")(currentFreshNameCreator)
      val flag :: flag_ :: Nil =
        createGetterSetter(owner, flagName, definitions.IntTpe, Literal(Constant(0)))

      // create var value holder
      val holder :: holder_ :: Nil =
        createGetterSetter(owner, TermName(lazyVal.name + "$value$"), lazyVal.tpt.tpe, EmptyTree)

      // create method with same name as lazy val
      val methodTerm = owner
        .newMethodSymbol(lazyVal.name, owner.pos, FINAL | SYNTHETIC)
        .setInfoAndEnter(lazyVal.tpt.tpe)
      val method = localTyper.typedPos(methodTerm.pos.focus)(
        DefDef(methodTerm, createMethodBody(owner, flag.symbol, holder.symbol, lazyVal.rhs))
      )

      List(flag, flag_, holder, holder_, method)
    }

    private def processBody(owner: Symbol, tmpl: Template): Template =
      treeCopy.Template(
        tmpl,
        tmpl.parents,
        tmpl.self,
        tmpl.body.flatMap { // TODO: this will create flag for each lazy val; will combined them all in single int
          case lazyVal @ ValDef(mods, _, _, _) if mods.isLazy =>
            rewriteLazyVal(owner, lazyVal)
          case x =>
            List(x)
        }
      )

    override def transform(tree: Tree): Tree =
      try {
        tree match {
          case cd @ ClassDef(_, _, _, tmpl @ Template(_, _, _)) =>
            val cd1 = super.transform(
              treeCopy.ClassDef(tree, cd.mods, cd.name, cd.tparams, processBody(cd.symbol, tmpl))
            )
            print(cd1)
            cd1
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
