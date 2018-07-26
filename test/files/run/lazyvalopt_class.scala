import scalaz.plugin.test._

object Test extends AsmpTest {
  override def code = """
                        |class LazyFoo {
                        |  lazy val foo: Int = 1
                        |  lazy val bar: Double = 2.0
                        |  lazy val baz: Boolean = false
                        |}
                        |
                        |object LazyFoo {
                        |  lazy val qux: String = "scalaz"
                        |}
                      """.stripMargin

  override def classes = List("LazyFoo")
}