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
      "\"-Dpartest.scalac_opts=" + s.replace("\"","\\\"") + '"'

    val partestArg = List.apply[Parser[String]](
      ( "-Dpartest.scalac_opts=" ~> StringBasic.examples()).map(scalac_opts),
      category,
      ("--" ~> category).map("--" ++ _),
      "--update-check".id,
      "--verbose".id,
      "--srcpath pending".id, // expand?
      testfile.map(_.getAbsolutePath), // must be last
    ).reduce(_ | _)

    Command("partest")(const((token(Space) ~> partestArg) *)) {
      (state, args) =>
        {
          val res = if (args.isEmpty) "tests/test:test"
          else s"tests/test:testOnly -- ${args mkString " "}"
      println(s"invoking $res")
      res
    } :: state
    }
  }

}
