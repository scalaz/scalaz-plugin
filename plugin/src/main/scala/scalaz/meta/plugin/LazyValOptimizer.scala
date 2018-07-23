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

    private def createMethodBody(owner: Symbol, flag: ValDef, holder: ValDef, body: Tree): Tree = {
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

    private def rewriteLazyVal(owner: Symbol, lazyVal: ValDef): List[Tree] = {
      import scala.reflect.internal.Flags._

      // remove lazy val
      owner.info.decls.unlink(lazyVal.symbol)

      // dynamic name
      val postfixName = "$" + lazyVal.pos.line + lazyVal.pos.column

      // create var flag
      val flagTerm = owner
        .newVariable(TermName("$lazyflag" + postfixName), owner.pos, PrivateLocal | SYNTHETIC)
        .setInfoAndEnter(global.definitions.IntTpe)
      val lazyFlag = newValDef(flagTerm, Literal(Constant(0)))()

      // create var value holder
      val valueTerm = owner
        .newVariable(TermName(lazyVal.symbol.name + "$value" + postfixName),
                     owner.pos,
                     PrivateLocal | SYNTHETIC | DEFAULTINIT)
        .setInfoAndEnter(lazyVal.tpt.tpe)
      val valueHolder = newValDef(valueTerm, EmptyTree)()

      // create method with same name as lazy val
      val methodTerm = owner
        .newMethodSymbol(lazyVal.name, owner.pos, FINAL | SYNTHETIC | ACCESSOR | METHOD)
        .setInfoAndEnter(lazyVal.tpt.tpe)
      val method =
        newDefDef(methodTerm, createMethodBody(owner, lazyFlag, valueHolder, lazyVal.rhs))()

      List(localTyper.typedValDef(lazyFlag),
           localTyper.typedValDef(valueHolder),
           localTyper.typedDefDef(method))
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
            super.transform(
              treeCopy.ClassDef(tree, cd.mods, cd.name, cd.tparams, processBody(cd.symbol, tmpl))
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
