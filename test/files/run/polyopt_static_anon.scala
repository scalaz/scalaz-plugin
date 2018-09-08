import scalaz.plugin.test._

object Test extends AsmpTest {
  override def extraSettings: String = super.extraSettings ++ " -P:scalaz-plugin:+polyopt"

  override def code = """
  |class Foo[A]()
  |
  |class Parent[A] {
  |  def someFunc[X]: Foo[A]            = new Foo[A]
  |  final def someFinalFunc[X]: Foo[A] = new Foo[A]
  |}
  |
  |object TestStatic {
  |  val foo = new Parent[String] {
  |    val a = 1
  |  }
  |  val boo = new Parent[String] { }
  |
  |  def f(p: Parent[String]): Unit = ???
  |
  |  val baz = f(new Parent[String] { })
  |}
  """.stripMargin

  override def classes = List("TestStatic$$anon$1", "TestStatic$$anon$2", "TestStatic$$anon$3")
}
