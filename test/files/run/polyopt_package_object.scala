import scalaz.plugin.test._

import java.io.{ BufferedReader, File, InputStreamReader }

object Test extends AsmpTest {

  override def code = """

class Foo[A]()

package test {
  object `package` {
    val boo: Int = 1

    def get[A]: Foo[A] = new Foo()

    def get1[A](a: A): List[A] = List(a)
  }
}
    """.stripMargin


  override def classes = "test/package$" :: Nil

}
