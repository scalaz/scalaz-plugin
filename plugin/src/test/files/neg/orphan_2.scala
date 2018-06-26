import scalaz.meta.Typeclass
import scalaz.meta.orphan

trait Monoid[A] extends Typeclass
object Monoid {
  implicit val intMonoid: Monoid[Int] = ???
}

object Test {
  def foo[A](foo: Monoid[A]): A = ???

  foo(new Monoid[Int] { }) // Bad, an orphan.

  foo(new Monoid[Int] @orphan { }) // Okay, an explicit orphan.
}