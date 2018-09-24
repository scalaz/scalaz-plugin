import sbt._
import internal.util.complete._

object Commandz {

  /** A command to invoke our partest, assuming that the tests live in the
   * given directory.
   *
   * This is shorter than I remember the scala build's being, and I didn't look
   * at that one while writing it, so it is presumably much lacking in features.
   */
  def partestCommand(base: File): Command = {
    import DefaultParsers._

    val categories = List("pos", "neg", "run")

    val category = categories.map(_.id).reduce(_ | _)

    val testfileExamples = {
      class Custom(pre: String) extends FileExamples(base, pre) {
        val knownDir = Set("files", "pos", "neg", "run", "target")

        override def withAddedPrefix(addedPrefix: String) =
          new Custom(pre + addedPrefix)

        override protected def files(directory: File) =
          super.files(directory).filter { s =>
            val f = file(base.getName + pre) / s
            (categories contains f.getParentFile.getName) && (
              (f.isDirectory && !(f.getName endsWith ".obj"))
              || f.getName.endsWith(".scala")
            )
          }
      }
      new Custom("")
    }

    val testfile = StringBasic
      .examples(testfileExamples)
      .map(new File(_))

    def scalac_opts(s: String) =
      "\"-Dpartest.scalac_opts=" + s.replace("\"", "\\\"") + '"'

    val partestArg = List
      .apply[Parser[String]](
        ("-Dpartest.scalac_opts=" ~> StringBasic.examples()).map(scalac_opts),
        category,
        ("--" ~> category).map("--" ++ _),
        "--update-check".id,
        "--verbose".id,
        "--srcpath pending".id, // expand?
        testfile.map(_.getAbsolutePath) // must be last
      )
      .reduce(_ | _)

    Command("partest")(const((token(Space) ~> partestArg) *)) { (state, args) =>
      {
        val res =
          if (args.isEmpty) "tests/test:test"
          else s"tests/test:testOnly -- ${args mkString " "}"
        println(s"invoking $res")
        res
      } :: state
    }
  }

  /** A command to launch the repl with given args and the plugin enabled.
   */
  val replCommand: Command =
    Command.args("repl", "<scalac args>") { (state, args) =>
      import Keys.{ state => _, _ }
      val extracted = Project.extract(state)
      val (state1, cp) =
        extracted.runTask(fullClasspath in Compile in LocalProject("plugin"), state)
      val (state2, pluginJar) =
        extracted.runTask(packageBin in Compile in LocalProject("plugin"), state1)
      // for some reason we need to add the classpath here... passing it to the
      // `runner` below lets us load scalac but nothing else (even with -usejavacp)
      val scalacOpts =
        "-cp" :: cp.map(_.data).mkString(":") :: s"-Xplugin:$pluginJar" :: args.toList

      val log              = extracted.get(sLog) // wrong logger, but works
      val (state3, runner) = extracted.runTask(Keys.runner in LocalProject("plugin"), state2)
      runner.run("scala.tools.nsc.MainGenericRunner", cp.map(_.data), scalacOpts, log).get

      state3
    }
}
