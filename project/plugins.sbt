addSbtPlugin("io.get-coursier"    % "sbt-coursier"             % "1.0.0-RC13")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.3.3")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "0.6.25")
addSbtPlugin("com.47deg"          % "sbt-microsites"           % "0.7.18")
addSbtPlugin("com.geirsson"       % "sbt-scalafmt"             % "1.5.1")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.9.0")
addSbtPlugin("com.geirsson"       % "sbt-ci-release"           % "1.2.1")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "1.5.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")

scalacOptions += "-language:_"
