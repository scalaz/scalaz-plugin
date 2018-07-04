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
                        |object Child extends Parent[String] {
                        |  override def someFunc[X]: Foo[String] = new Foo[String]
                        |}
                      """.stripMargin

  override def classes = List("Child$")
}
