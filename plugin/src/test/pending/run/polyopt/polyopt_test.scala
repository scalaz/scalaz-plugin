
//object Foo {
//  val value: List[Any] = Nil
//  def get[A]: List[A] = value.asInstanceOf[List[A]]
//}

class Foo[A]()

object FooBar {
  val boo: Int = 1

  def get[A]: Foo[A] = new Foo()

  def get1[A](a: A): List[A] = List(a)
}


