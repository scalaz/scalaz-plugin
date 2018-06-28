//object Foo {
//  val value: List[Any] = Nil
//  def get[A]: List[A] = value.asInstanceOf[List[A]]
//}

class Foo[A]()

object FooBar {
  val boo: Int = 1

  def get[A]: Foo[A] = new Foo()

  def get1[A](a: A): List[A] = List(a)
}

class Parent[A] {
  def someFunc[X]: Foo[A] = new Foo[A]
  final def someFinalFunc[X]: Foo[A] = new Foo[A]
}

object ChildObj extends Parent[String]

object ChildObjRedef extends Parent[String] {
  val a = 1
  override def someFunc[X]: Foo[String] = new Foo[String]
}

object TestStatic {
  // owner chain : List(value foo, object TestStatic, package <empty>, package <root>)
  val foo = new Parent[String] {
    val a = 1
  }

  // List(value foo, object TestStatic, package <empty>, package <root>)
  def boo(p: Parent[String]): Unit = ???
  val foo1 = boo(new Parent[String] { })

  class Test {
    val boo = new Parent[String] {
      val b = 1
    }

    object Bad
  }

  var VarIsNotStatic  = new Parent[String] {
    val a = 1
  }

  class Boo(val x: Parent[String] = new Parent[String] {
    val a = 1
  })
}

package test {
  object `package` {
    val boo: Int = 1

    def get[A]: Foo[A] = new Foo()

    def get1[A](a: A): List[A] = List(a)
  }
}

import java.io.{ BufferedReader, File, InputStreamReader }

object Test {
  def decompile(name: String): Unit = {
    val path = new File(s"src/test/files/run/polyopt-run.obj/$name.class").getAbsolutePath

    val p = new ProcessBuilder()
      .command("javap", "-c", path)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .waitFor()
  }

  def main(args: Array[String]): Unit = {
//    println(new File("src/test/pending/run/polyopt-run.obj/").list().toList)
    decompile("FooBar$")
    decompile("ChildObj$")
    decompile("ChildObjRedef$")
    decompile("TestStatic$$anon$1")
  }
}
