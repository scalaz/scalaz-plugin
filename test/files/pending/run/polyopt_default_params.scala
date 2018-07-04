import scalaz.plugin.test._

object Test extends AsmpTest {
  override def code = """
     |class Foo[A]()
     |
     |class Parent[A] {
     |  def someFunc[X]: Foo[A]            = new Foo[A]
     |  final def someFinalFunc[X]: Foo[A] = new Foo[A]
     |}
     |
     |object Scope {
     |  class Foo(val x: Parent[String] = new Parent[String] { })
     |}
    """.stripMargin


  override def classes = List("Scope$Foo$$anon$1")
}
