import scalaz.meta.Typeclass
import scalaz.meta.orphan

import scala._

object orphan_definitions {
  trait Foo[A] extends Typeclass
  object Foo {
    // This is okay, typeclass companion.
    implicit val foo: Foo[Int] = new Foo[Int] {} // ok
  }

  class Bar
  object Bar {
    // This is okay, type companion.
    implicit val foo: Foo[Bar] = new Foo[Bar] {} // ok
  }

  // Not okay, orphan instance.
  implicit val foo1: Foo[Long] = new Foo[Long] {} // bad

  object Other {
    // Not okay, orphan instance.
    implicit val foo2: Foo[String] = new Foo[String] {} // bad
  }

  object Something {
    // This is okay, an explicit orphan.
    implicit val foo3: Foo[String] = new Foo[String] @orphan {} // ok
  }
}
