import scalaz.meta.Typeclass
import scalaz.meta.orphan

import scala._

object orphan_definitions {
  trait Foo[A] extends Typeclass
  object Foo {
    implicit val foo: Foo[Int] = new Foo[Int] { } // ok
  }

  implicit val foo1: Foo[Long] = new Foo[Long] { } // bad

  object Other {
    implicit val foo2: Foo[String] = new Foo[String] { } // bad
  }

  object Something {
    @orphan implicit val foo3: Foo[String] = new Foo[String] { } // ok, explicit orphan
  }
}
