object Foo {
  def foo1: Nothing = sys.error("test")
  def foo2(i: Int): Nothing = sys.error(s"test $i")
  def foo3(i: Nothing): Nothing = sys.error(s"test $i")
}