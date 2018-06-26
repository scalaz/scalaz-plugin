import java.io.{BufferedReader, InputStreamReader}

object Test {
  def main(args: Array[String]): Unit = {
    val p = new ProcessBuilder().command("javap", "FooBar$.class").redirectOutput(ProcessBuilder.Redirect.INHERIT).start().waitFor()
  }
}