package scalaz.plugin.test

import scala.tools.partest._

/** A test which compiles some given `code`, and compares the produced bytecode
  * for specified `classes` with the check file.
  */
abstract class AsmpTest extends DirectTest {
  /** The code to compile. */
  def code: String

  /** The binary names of the classes to validate. */
  def classes: List[String]

  override def extraSettings =
    s"-usejavacp -Xplugin:${sys.props("scalaz.plugin.jar")} -Ygen-asmp ${testOutput.path}"

  override def show() = {
    compile()
    classes.foreach { cls =>
      println(io.Source.fromFile(testOutput / s"$cls.asmp").mkString)
    }
  }
}