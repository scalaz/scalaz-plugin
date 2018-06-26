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
//    treeBrowsers.create().browse("aa", List(unit))
    new MyTransformer(unit)

  def replaceTree[T <: Tree](prev: Tree, next: T): T =
    atPos(prev.pos.makeTransparent)(next)
      .setAttachments(prev.attachments)

  def isSelect(tree: Tree): Boolean = tree match {
    case TypeApply(fun, args) => isSelect(fun)
    case Select(qualifier, _) => isSelect(qualifier)
    case Ident(a)             => true
    case This(_)              => true
    case Super(qual, _)       => isSelect(qual)
    case _                    => false
  }

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree =
      try {
        tree match {
          case mod @ ModuleDef(_, _, tmpl @ Template(_, _, body)) =>
            val newImpl = treeCopy.Template(
              tmpl,
              tmpl.parents,
              tmpl.self,
              body.flatMap {
                case fun @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
                    if !mods.isSynthetic && !fun.symbol.isConstructor && !fun.symbol.isAccessor && !isSelect(rhs) =>
                  val params         = tparams.map(_.symbol)
                  val argTypes       = vparamss.flatten.map(_.tpt.tpe)
                  val usedTypeParams = params.filter(p => argTypes.exists(_.contains(p)))

                  if (usedTypeParams.isEmpty) {
                    val name1 = freshTermName("local$")(currentFreshNameCreator)
                    val tpt1  = tpt.substituteTypes(tparams.map(_.symbol), tparams.map(_ => definitions.AnyTpe))
                    val rhs1  = rhs.substituteTypes(tparams.map(_.symbol), tparams.map(_ => definitions.AnyTpe))
                    import scala.tools.nsc.symtab.Flags._

                    val val1_symbol = mod.symbol.moduleClass
                      .newTermSymbol(name1, rhs.pos, FINAL | LOCAL | SYNTHETIC)
                      .setInfoAndEnter(tpt1.tpe)

                    val val1 = newValDef(val1_symbol, rhs1)(Modifiers(0), name1, tpt1)

                    val fun1 = treeCopy.DefDef(
                      fun,
                      fun.mods,
                      fun.name,
                      fun.tparams,
                      fun.vparamss,
                      fun.tpt,
                      TypeApply(Select(Select(This(mod.symbol.moduleClass), val1_symbol), "asInstanceOf"), List(tpt))
                    )

                    List(localTyper.typedValDef(val1), localTyper.typedDefDef(fun1))
                  } else List(fun)
                case x => List(x)
              }
            )

            val result = treeCopy.ModuleDef(tree, mod.mods, mod.name, newImpl)

            super.transform(result)
          case _ => super.transform(tree)
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(System.err)
          super.transform(tree)
      }
  }
}
