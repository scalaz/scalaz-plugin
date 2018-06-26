import java.io.{ BufferedReader, File, InputStreamReader }

object Test {
  def main(args: Array[String]): Unit = {
    val path = new File("src/test/files/run/polyopt-run.obj/FooBar$.class").getAbsolutePath

    val p = new ProcessBuilder()
      .command("javap", "-c", path)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .waitFor()
  }
}
