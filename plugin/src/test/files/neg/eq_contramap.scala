import scalaz.meta.Typeclass
import scalaz.meta.orphan

import scala._

object eq_contramap {
  trait Eq[A] extends Typeclass {
    def contramap[B](f: B => A): Eq[B]
  }
  object Eq {
    val eqInt: Eq[Int] = ???
    val eqLong: Eq[Long] = eqInt.contramap(_.toInt) // ok
  }

  val eqLonger: Eq[Long] = Eq.eqInt.contramap(_.toInt) // bad
}
