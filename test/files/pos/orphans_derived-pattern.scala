import scalaz.meta.Typeclass

trait Foo[A] extends FooDerived[A] with Typeclass
trait FooDerived[A] { self: Foo[A] => }
object Foo {
  implicit def foo[A]: Foo[A] = new Foo[A] { }
}