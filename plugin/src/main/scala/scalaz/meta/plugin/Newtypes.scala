package scalaz.meta.plugin

import scalaz.Scalaz._
import scalaz._

import scala.reflect.internal.util.FreshNameCreator
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }

abstract class Newtypes extends AnnotationTransformers {
  val global: Global
  val scalazDefns: Definitions { val global: Newtypes.this.global.type }

  import global._
  import scalazDefns.{ global => _ }

  sealed abstract class Error                                       extends Product with Serializable
  final case class IllegalSupertypes(unsupported: List[Tree])       extends Error
  final case object NoConstructor                                   extends Error
  final case object MultiArgConstructor                             extends Error
  final case class ValNotSupported(vd: ValDef)                      extends Error
  final case class PublicConstructorValInNonObject(v: ValDef)       extends Error
  final case class IllegalDefinition(d: Tree)                       extends Error
  final case class OptimizedOpsInNonObject(m: NonEmptyList[DefDef]) extends Error

  type Validated[A] = Validation[NonEmptyList[Error], A]
  def ok[A](a: A): Validated[A]       = Validation.success(a)
  def fail[A](e: Error): Validated[A] = Validation.failureNel(e)

  def showError(cd: ClassDef, e: Error): Unit = e match {
    case IllegalSupertypes(unsupported) =>
      globalError(
        cd.pos,
        s"newtypes do not support inheritance; illegal supertypes: ${unsupported.mkString(", ")}"
      )
    case NoConstructor =>
      globalError(cd.pos, "Failed to locate constructor")
    case MultiArgConstructor =>
      globalError(cd.pos, "Unsupported constructor, must have exactly one argument")
    case ValNotSupported(vd) =>
      globalError(vd.pos, "val definitions not supported, use def instead")
    case IllegalDefinition(t) =>
      globalError(t.pos, s"Illegal definition in newtype: $t ${showRaw(t)}")
    case OptimizedOpsInNonObject(m) =>
      globalError(m.head.pos, "Methods can only be defined for newtypes defined in an object")
    case PublicConstructorValInNonObject(v) =>
      globalError(v.pos,
                  List(
                    "Fields can only be defined for newtypes defined in an object",
                    s"Consider defining as: private val ${v.name.decodedName}"
                  ).mkString("\n"))
  }

  object MyTransformer extends AnnotatedClassTransformer(_ == scalazDefns.NewtypeAnnotation) {
    val debug    = true
    val debugRaw = true

    override def transformAnnotated(
      clsDef: ClassDef,
      modDef: Option[ModuleDef]
    ): List[Tree] = {
//      println(s"annotation.args = ${annotation.args}")

      val result = runAnnotation("newtypes", false, clsDef, modDef)
      if (debug) scala.Predef.println(s"Expanded @newtypes :\n" + show(result))
      if (debugRaw) scala.Predef.println(s"Expanded @newtypes (raw):\n" + showRaw(result))
      result
    }

    def runAnnotation(annotationName: String,
                      subtype: Boolean,
                      clsDef: ClassDef,
                      modDef: Option[ModuleDef]): List[Tree] =
      modDef match {
        case None         => runClass(clsDef, subtype)
        case Some(modDef) => runClassWithObj(clsDef, modDef, subtype)
      }

    def runClass(clsDef: ClassDef, subtype: Boolean): List[Tree] =
      runClassWithObj(clsDef, q"object ${clsDef.name.toTermName}".asInstanceOf[ModuleDef], subtype)

    def runClassWithObj(clsDef: ClassDef, modDef: ModuleDef, subtype: Boolean): List[Tree] =
      getConstructor(clsDef.impl.body).toEither match {
        case Right(constructor) =>
          extractConstructorValDef(constructor).toEither match {
            case Right(valDef) =>
              // Converts [F[_], A] to [F, A]; needed for applying the defined type params.
              val tparamNames: List[TypeName] = clsDef.tparams.map(_.name)
              // Type params with variance removed for building methods.
              val tparamsNoVar: List[TypeDef] = clsDef.tparams.map(
                td => TypeDef(Modifiers(Flag.PARAM), td.name, td.tparams, td.rhs)
              )
              val tparamsWild = tparamsNoVar.map {
                case TypeDef(mods, _, args, tree) => TypeDef(mods, typeNames.WILDCARD, args, tree)
              }
              // Ensure we're not trying to inherit from anything.
              validateParents(clsDef.impl.parents)
              // Build the type and object definitions.
              generateNewType(clsDef,
                              modDef,
                              valDef,
                              tparamsNoVar,
                              tparamNames,
                              tparamsWild,
                              subtype)
            case Left(errs) =>
              errs.foreach(e => showError(clsDef, e))
              List(clsDef, modDef)
          }

        case Left(errs) =>
          errs.foreach(e => showError(clsDef, e))
          List(clsDef, modDef)
      }

    def generateNewType(clsDef: ClassDef,
                        modDef: ModuleDef,
                        valDef: ValDef,
                        tparamsNoVar: List[TypeDef],
                        tparamNames: List[TypeName],
                        tparamsWild: List[TypeDef],
                        subtype: Boolean,
    ): List[Tree] = {
      val ModuleDef(objMods, objName, Template(objParents, objSelf, objDefs)) = modDef
      val typeName                                                            = clsDef.name
      val clsName                                                             = clsDef.name.decodedName
      val reprType                                                            = valDef.tpt
      val typesTraitName                                                      = TypeName(s"${clsName.decodedName}__Types")
      val tparams                                                             = clsDef.tparams

      // We need to know if the newtype is defined in an object so we can report
      // an error message if methods are defined on it (otherwise, the user will
      // get a cryptic error of 'value class may not be a member of another class'
      // due to our generated extension methods.
      val isDefinedInObject = true // currentOwner.isModuleClass
      val optimizeOps       = false

      val opsDef = maybeGenerateOpsDef(clsDef,
                                       valDef,
                                       tparamsNoVar,
                                       tparamNames,
                                       isDefinedInObject,
                                       optimizeOps)
      val companionExtraDefs = opsDef.map(
        opsDef =>
//        maybeGenerateApplyMethod(clsDef, valDef, tparamsNoVar, tparamNames) ++
//        generateUnapplyMethod(clsDef, valDef, tparamsNoVar, tparamNames) ++
        opsDef
//        generateDerivingMethods(tparamsNoVar, tparamNames, tparamsWild, new FreshNameCreator())
      )

      companionExtraDefs.toEither match {
        case Right(companionExtraDefs) =>
          val newtypeObjParents = objParents :+ tq"$typesTraitName"
          val newtypeObjDef = ModuleDef(
            objMods,
            objName,
            Template(newtypeObjParents, objSelf, objDefs ++ companionExtraDefs)
          )

          // Note that we use an abstract type alias
          // `type Type <: Base with Tag` and not `type Type = ...` to prevent
          // scalac automatically expanding the type alias.
          // Also, Scala 2.10 doesn't support objects having abstract type members, so we have to
          // use some indirection by defining the abstract type in a trait then having
          // the companion object extend the trait.
          // See https://github.com/scala/bug/issues/10750

          val baseTypeDef = mkBaseTypeDef(clsDef, reprType, subtype)
          val typeTypeDef = mkTypeTypeDef(clsDef, tparamNames, subtype)

          if (tparams.isEmpty) {
            List(
              q"type $typeName = $objName.Type",
              q"""
              trait $typesTraitName {
                type Repr = $reprType
                $baseTypeDef
                trait Tag extends ${definitions.AnyTpe}
                ${mkTypeTypeDef(clsDef, tparamNames, subtype)}
              }""",
              newtypeObjDef
            )
          } else {
            List(
              q"type $typeName[..$tparams] = ${typeName.toTermName}.Type[..$tparamNames]",
              q"""
              trait $typesTraitName {
                type Repr[..$tparams] = $reprType
                $baseTypeDef
                trait Tag[..$tparams] extends ${definitions.AnyTpe}
                $typeTypeDef
              }""",
              newtypeObjDef
            )
          }
        case Left(errs) =>
          for (e <- errs) showError(clsDef, e)
          List(clsDef, modDef)
      }
    }

    def mkBaseTypeDef(clsDef: ClassDef, reprType: Tree, subtype: Boolean): Tree = {
      val refinementName = TypeName(s"__${clsDef.name.decodedName.toString}__newtype")
      (clsDef.tparams, subtype) match {
        case (_, false) =>
          q"type Base             = ${definitions.AnyTpe} { type $refinementName } "
        case (Nil, true)     => q"type Base             = $reprType"
        case (tparams, true) => q"type Base[..$tparams] = $reprType"
      }
    }

    def mkTypeTypeDef(clsDef: ClassDef, tparamsNames: List[TypeName], subtype: Boolean): Tree =
      (clsDef.tparams, subtype) match {
        case (Nil, false)     => q"type Type             <: Base with Tag"
        case (tparams, false) => q"type Type[..$tparams] <: Base with Tag[..$tparamsNames]"
        case (Nil, true)      => q"type Type             <: Base with Tag"
        case (tparams, true) =>
          q"type Type[..$tparams] <: Base[..$tparamsNames] with Tag[..$tparamsNames]"
      }

    def maybeGenerateApplyMethod(clsDef: ClassDef,
                                 valDef: ValDef,
                                 tparamsNoVar: List[TypeDef],
                                 tparamNames: List[TypeName]): List[Tree] =
      if (!clsDef.mods.hasFlag(Flag.CASE)) Nil
      else
        List(
          if (tparamsNoVar.isEmpty) {
            q"def apply(${valDef.name}: ${valDef.tpt}): ${clsDef.name} = ${valDef.name}.asInstanceOf[${clsDef.name}]"
          } else {
            q"""
          def apply[..$tparamsNoVar](${valDef.name}: ${valDef.tpt}): ${clsDef.name}[..$tparamNames] =
            ${valDef.name}.asInstanceOf[${clsDef.name}[..$tparamNames]]
        """
          }
        )

    def generateUnapplyMethod(clsDef: ClassDef,
                              valDef: ValDef,
                              tparamsNoVar: List[TypeDef],
                              tparamNames: List[TypeName]): List[Tree] =
      // Note that our unapply method should Some since its isEmpty/get is constant.
      List(
        if (tparamsNoVar.isEmpty) {
          q"""def unapply(x: ${clsDef.name}): Some[${valDef.tpt}] =
              Some(x.asInstanceOf[${valDef.tpt}])"""
        } else {
          q"""def unapply[..$tparamsNoVar](x: ${clsDef.name}[..$tparamNames]): Some[${valDef.tpt}] =
              Some(x.asInstanceOf[${valDef.tpt}])"""
        }
      )

    def maybeGenerateOpsDef(clsDef: ClassDef,
                            valDef: ValDef,
                            tparamsNoVar: List[TypeDef],
                            tparamNames: List[TypeName],
                            isDefinedInObject: Boolean,
                            optimizeOps: Boolean): Validated[List[Tree]] = {
      val extensionMethods: Validated[List[Tree]] =
        (maybeGenerateValMethod(clsDef, valDef, isDefinedInObject, optimizeOps).map(_.toList)
          |@| getInstanceMethods(clsDef, isDefinedInObject, optimizeOps))(_ ++ _)

      extensionMethods.map { extensionMethods =>
        if (extensionMethods.isEmpty) {
          Nil
        } else {
          val parent = if (optimizeOps) typeOf[AnyVal].typeSymbol else typeOf[AnyRef].typeSymbol
          // Note that we generate the implicit class for extension methods and the
          // implicit def to convert `this` used in the Ops to our newtype value.
          if (clsDef.tparams.isEmpty) {
            List(
              q"""
              implicit final class Ops$$newtype(private[this] val $$this$$: Type) extends $parent {
                ..$extensionMethods
              }
            """
            )
          } else {
            List(
              q"""
              implicit final class Ops$$newtype[..${clsDef.tparams}](
                private[this] val $$this$$: Type[..$tparamNames]
              ) extends $parent {
                ..$extensionMethods
              }
            """
            )
          }
        }
      }
    }

    def maybeGenerateValMethod(clsDef: ClassDef,
                               valDef: ValDef,
                               isDefinedInObject: Boolean,
                               optimizeOps: Boolean): Validated[Option[Tree]] =
      if (!shouldGenerateValMethod(clsDef, valDef)) {
        ok(None)
      } else if (!isDefinedInObject && optimizeOps) {
        fail(PublicConstructorValInNonObject(valDef))
      } else {
        ok(Some(q"def ${valDef.name}: ${valDef.tpt} = $$this$$.asInstanceOf[${valDef.tpt}]"))
      }

    // We should expose the constructor argument as an extension method only if
    // it was defined as a public param.
    def shouldGenerateValMethod(clsDef: ClassDef, valDef: ValDef): Boolean =
      clsDef.impl.body.collectFirst {
        case vd: ValDef
            if (vd.mods.hasFlag(Flag.CASEACCESSOR) || vd.mods.hasFlag(Flag.PARAMACCESSOR))
              && !vd.mods.hasFlag(Flag.PRIVATE)
              && vd.name == valDef.name =>
          ()
      }.isDefined

    def generateDerivingMethods(tparamsNoVar: List[TypeDef],
                                tparamNames: List[TypeName],
                                tparamsWild: List[TypeDef],
                                freshNames: FreshNameCreator): List[Tree] =
      if (tparamsNoVar.isEmpty) {
        List(q"def deriving[TC[_]](implicit ev: TC[Repr]): TC[Type] = ev.asInstanceOf[TC[Type]]")
      } else {
        // Creating a fresh type name so it doesn't collide with the tparams passed in.
        val TC = freshTypeName("TC")(freshNames)
        List(
          q"""
          def deriving[$TC[_], ..$tparamsNoVar](
            implicit ev: $TC[Repr[..$tparamNames]]
          ): $TC[Type[..$tparamNames]] = ev.asInstanceOf[$TC[Type[..$tparamNames]]]
        """,
          q"""
          def derivingK[$TC[_[..$tparamsWild]]](implicit ev: $TC[Repr]): $TC[Type] =
            ev.asInstanceOf[$TC[Type]]
        """
        )
      }

    def validateParents(parents: List[Tree]): Validated[Unit] = {
      val ignoredExtends = List(tq"scala.Product", tq"scala.Serializable", tq"scala.AnyRef")
      val unsupported    = parents.filterNot(t => ignoredExtends.exists(t.equalsStructure))
      if (unsupported.isEmpty) ok(())
      else fail(IllegalSupertypes(unsupported))
    }

    def getConstructor(body: List[Tree]): Validated[DefDef] =
      body.collectFirst {
        case dd: DefDef if dd.name == termNames.CONSTRUCTOR =>
          ok(dd)
      }.getOrElse(fail(NoConstructor))

    def extractConstructorValDef(ctor: DefDef): Validated[ValDef] =
      ctor.vparamss match {
        case List(List(vd)) => ok(vd)
        case _              => fail(MultiArgConstructor)
      }

    def getInstanceMethods(clsDef: ClassDef,
                           isDefinedInObject: Boolean,
                           optimizeOps: Boolean): Validated[List[DefDef]] = {
      val res = clsDef.impl.body
        .traverse[Validated, List[DefDef]] {
          case vd: ValDef =>
            if (vd.mods.hasFlag(Flag.CASEACCESSOR) || vd.mods.hasFlag(Flag.PARAMACCESSOR))
              ok(Nil)
            else fail(ValNotSupported(vd))
          case dd: DefDef =>
            if (dd.name == termNames.CONSTRUCTOR) ok(Nil) else ok(List(dd))
          case x if x.isEmpty => ok(Nil)
          case x              => fail(IllegalDefinition(x))
        }
        .map(_.flatten)

      res.toEither match {
        case Right(methods) if methods.nonEmpty && !isDefinedInObject && optimizeOps =>
          fail(OptimizedOpsInNonObject(NonEmptyList(methods.head, methods.tail: _*)))
        case _ => res
      }
    }
  }
}
