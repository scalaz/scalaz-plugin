import scalaz.meta.Typeclass
import scalaz.meta.orphan

import scala._

object orphan_hk {
  trait Foo[A[_]] extends Typeclass
  object Foo {
    // This is okay, typeclass companion.
    implicit val foo: Foo[List] = new Foo[List] {}
  }

  class Bar[A]
  object Bar {
    // This is okay, type companion.
    implicit val foo: Foo[Bar] = new Foo[Bar] {}
  }

  // Not okay, orphan instance.
  implicit val foo1: Foo[Bar] = new Foo[Bar] {}

  object Other {
    // Not okay, orphan instance.
    implicit val foo2: Foo[Bar] = new Foo[Bar] {}
  }

  object Something {
    // This is okay, an explicit orphan.
    implicit val foo3: Foo[Bar] = new Foo[Bar] {} : @orphan
  }
}
