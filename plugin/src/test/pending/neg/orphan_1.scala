import scalaz.meta.Typeclass
import scalaz.meta.orphan

trait Monoid[A] extends Typeclass
object Monoid {
  implicit val intMonoid: Monoid[Int] = ???
}

trait Foo[A] {
  implicit val monoid: Monoid[A]
}

object Test {
  def foo[A](foo: Foo[A]): A = ???

  foo[Int](new Foo[Int] {
    implicit val monoid: Monoid[Int] = new Monoid[Int] { } // Not good, orphan.
  })
}