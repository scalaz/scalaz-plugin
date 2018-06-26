class Foo[A]()

object FooBar {
  val boo: Int = 1

  def get[A]: Foo[A] = new Foo()

  def get1[A](a: A): List[A] = List(a)
}
