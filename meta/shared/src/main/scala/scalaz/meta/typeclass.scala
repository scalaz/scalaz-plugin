package scalaz.meta

import com.github.ghik.silencer.silent
import scala.Any
import scala.annotation.StaticAnnotation

trait Typeclass

class orphan extends StaticAnnotation

@silent
class minimal(defns: Any*) extends StaticAnnotation

class instances extends StaticAnnotation

class unmixin extends StaticAnnotation

object features {
  @scala.annotation.meta.languageFeature("enable orphan instances", enableRequired = true)
  sealed trait orphans
  object orphans extends orphans
}

object enable {
  implicit lazy val orphans: features.orphans = features.orphans
}
