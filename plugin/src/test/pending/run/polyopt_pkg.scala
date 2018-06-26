class Foo[A]()

package test {
  object `package` {
    val boo: Int = 1

    def get[A]: Foo[A] = new Foo()

    def get1[A](a: A): List[A] = List(a)
  }
}

import java.io.{ BufferedReader, File, InputStreamReader }

object Test {
  def main(args: Array[String]): Unit = {
    val path = new File("src/test/pending/run/polyopt_pkg-run.obj/test/package$.class").getAbsolutePath

    val p = new ProcessBuilder()
      .command("javap", "-c", path)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .waitFor()
  }
}
