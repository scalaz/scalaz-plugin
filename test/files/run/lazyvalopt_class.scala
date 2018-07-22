import scalaz.plugin.test._

object Test extends AsmpTest {
  override def code = """
                        |class LazyFoo {
                        |  lazy val foo: Int = 1
                        |  lazy val bar: String = "scalaz"
                        |}
                      """.stripMargin

  override def classes = List("LazyFoo")
}