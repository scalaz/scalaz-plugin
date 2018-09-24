package scalaz.plugin

import scala.tools.nsc.Global

trait Utils {
  val global: Global
  import global._

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
    if (s == null) "NullSymbol"
    else {
      s.toString() + "[" + symbolMethods
        .filter(m => m.invoke(s).asInstanceOf[Boolean])
        .map(_.getName)
        .mkString(", ") + "]"
    }

  def isStaticScope(sym: Symbol, currentOwner: Symbol): Boolean = {
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

  def isParameterlessPolymorphicMethod(s: Symbol): Boolean =
    if (s != null && s.isMethod && !s.isConstructor) {
      val m = s.asMethod
      m.typeParams.nonEmpty && m.paramLists.isEmpty
    } else false

  def collectParameterlessPolymorphicMethods(tmpl: Template): List[MethodSymbol] =
    tmpl.body.map(_.symbol).filter(isParameterlessPolymorphicMethod).map(_.asMethod)

  implicit final class Ops[A](val self: A) {
    def opt[B](f: PartialFunction[A, B]): Option[B] = f.lift(self)
  }
}
