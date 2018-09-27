package scalaz.plugin

import scalaz.plugin.FieldBuster.Path

object ResolutionFixDb {
  import FieldBuster._

  private[this] def add(gn: String, an: String, l: String): ((String, String), List[Path]) =
    (gn, an) -> l.stripMargin.split('\n').map(_.trim()).filter(_.nonEmpty).map(parsePath).toList

  val db: Map[(String, String), List[Path]] = Map(
    add(
      "scala.tools.nsc.doc.DocFactory$compiler$",
      "scala.tools.nsc.doc.ScaladocGlobal$$anon$1",
        """_root_/analyzer@scala.tools.nsc.doc.ScaladocGlobal
          |_root_/lastSeenContext@scala.tools.nsc.Global/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/namerFactory$module@scala.tools.nsc.doc.ScaladocGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
          |_old_/typerFactory$module@scala.tools.nsc.doc.ScaladocGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
          |_old_/NoImplicitInfo@scala.tools.nsc.doc.ScaladocGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
          |_old_/NoContext$module@scala.tools.nsc.doc.ScaladocGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/Context$module@scala.tools.nsc.doc.ScaladocGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context$
          |_root_/lastSeenContext@scala.tools.nsc.Global/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter"""
    ),
    add(
      "xsbt.ZincCompiler",
      "scala.tools.nsc.Global$$anon$1",
        """_root_/analyzer@scala.tools.nsc.Global
          |_root_/delambdafy$module@scala.tools.nsc.Global/analyzer@scala.tools.nsc.transform.Delambdafy
          |_root_/lastSeenContext@scala.tools.nsc.Global/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/namerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
          |_old_/typerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
          |_old_/NoImplicitInfo@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
          |_old_/NoContext$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/Context$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context$
          |_root_/lastSeenContext@scala.tools.nsc.Global/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter"""
    ),
    add(
      "scala.tools.nsc.Global",
      "scala.tools.nsc.Global$$anon$1",
        """_root_/analyzer@scala.tools.nsc.Global
          |_root_/delambdafy$module@scala.tools.nsc.Global/analyzer@scala.tools.nsc.transform.Delambdafy
          |_root_/lastSeenContext@scala.tools.nsc.Global/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/namerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
          |_old_/typerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
          |_old_/NoImplicitInfo@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
          |_old_/NoContext$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context
          |_old_/Context$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context$
          |_root_/lastSeenContext@scala.tools.nsc.Global/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter"""
    ),
    add(
      "scala.tools.partest.nest.PartestGlobal",
      "scala.tools.nsc.Global$$anon$1",
      """_root_/analyzer@scala.tools.nsc.Global
        |_root_/delambdafy$module@scala.tools.nsc.Global/analyzer@scala.tools.nsc.transform.Delambdafy
        |_root_/lastSeenContext@scala.tools.nsc.Global/$outer@scala.tools.nsc.typechecker.Contexts$Context
        |_old_/namerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
        |_old_/typerFactory$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
        |_old_/NoImplicitInfo@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
        |_old_/NoContext$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context
        |_old_/Context$module@scala.tools.nsc.Global$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context$
        |_root_/lastSeenContext@scala.tools.nsc.Global/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter
        |"""
    ),
    add(
      "scala.tools.nsc.interpreter.IMain$$anon$1",
      "scala.tools.nsc.interpreter.ReplGlobal$$anon$1",
      """_root_/analyzer@scala.tools.nsc.interpreter.IMain$$anon$1
        |_root_/delambdafy$module@scala.tools.nsc.Global/analyzer@scala.tools.nsc.transform.Delambdafy
        |_root_/lastSeenContext@scala.tools.nsc.Global/$outer@scala.tools.nsc.typechecker.Contexts$Context
        |_old_/namerFactory$module@scala.tools.nsc.interpreter.ReplGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
        |_old_/typerFactory$module@scala.tools.nsc.interpreter.ReplGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
        |_old_/NoImplicitInfo@scala.tools.nsc.interpreter.ReplGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
        |_old_/NoContext$module@scala.tools.nsc.interpreter.ReplGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context
        |_old_/Context$module@scala.tools.nsc.interpreter.ReplGlobal$$anon$1/$outer@scala.tools.nsc.typechecker.Contexts$Context$
        |_root_/lastSeenContext@scala.tools.nsc.Global/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter
        |"""
    ),
    add(
      "scala.tools.nsc.interpreter.PresentationCompilation$$anon$1",
      "scala.tools.nsc.interactive.Global$$anon$4",
      """_root_/analyzer@scala.tools.nsc.interactive.Global
        |_root_/delambdafy$module@scala.tools.nsc.Global/analyzer@scala.tools.nsc.transform.Delambdafy
        |_old_/namerFactory$module@scala.tools.nsc.interactive.Global$$anon$4/$outer@scala.tools.nsc.typechecker.Analyzer$namerFactory$
        |_old_/typerFactory$module@scala.tools.nsc.interactive.Global$$anon$4/$outer@scala.tools.nsc.typechecker.Analyzer$typerFactory$
        |_old_/NoImplicitInfo@scala.tools.nsc.interactive.Global$$anon$4/$outer@scala.tools.nsc.typechecker.Implicits$ImplicitInfo
        |_old_/NoContext$module@scala.tools.nsc.interactive.Global$$anon$4/$outer@scala.tools.nsc.typechecker.Contexts$Context
        |_old_/Context$module@scala.tools.nsc.interactive.Global$$anon$4/$outer@scala.tools.nsc.typechecker.Contexts$Context$
        |_old_/NoContext$module@scala.tools.nsc.interactive.Global$$anon$4/scala$tools$nsc$typechecker$Contexts$Context$$_reporter@scala.tools.nsc.typechecker.Contexts$Context/$outer@scala.tools.nsc.typechecker.Contexts$ContextReporter
        |"""
    )
  )
}
