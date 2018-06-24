package scalaz.meta

import com.github.ghik.silencer.silent
import scala.Any
import scala.annotation.StaticAnnotation

trait Typeclass

class orphan extends StaticAnnotation

@silent
class minimal(defns: Any*) extends StaticAnnotation