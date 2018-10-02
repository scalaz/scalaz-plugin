import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

import Scalaz._

cancelable in Global := true

inThisBuild(
  List(
    crossScalaVersions := List("2.12.6", "2.12.4"),
    organization := "org.scalaz",
    homepage := Some(url("https://github.com/scalaz/scalaz-plugin")),
    licenses := List("LGPL 3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    developers := List(
      Developer(
        "alexknvl",
        "Alexander Konovalov",
        "alex.knvl@gmail.com",
        url("https://alexknvl.com/")
      ),
      Developer(
        "hrhino",
        "Harrison Houghton",
        "self@haromorphism.net",
        url("https://haromorphism.net/")
      )
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
scalacOptions ++= List("-Xplugin-require:macroparadise")

lazy val meta = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(stdSettings("plugin-library"))

lazy val plugin = (project in file("plugin"))
  .settings(
    name := "scalaz-plugin",
    crossVersion := CrossVersion.full,
    libraryDependencies ++= List(
      scalaOrganization.value % "scala-reflect"  % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
    )
  )
  .dependsOn(meta.jvm)

lazy val PluginDependency: List[Def.Setting[_]] = List(scalacOptions ++= {
  val jar = (packageBin in Compile in plugin).value
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
})

lazy val example = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(stdSettings("example"))
  .settings(publishArtifact := false, skip in publish := true)
  .dependsOn(meta)
  .settings(PluginDependency: _*)

lazy val tests = (project in file("test"))
  .dependsOn(plugin)
  .settings(publishArtifact := false, skip in publish := true)
  .settings(
    partestFramework,
    libraryDependencies ++= List(
      scalaOrganization.value % "scala-reflect"  % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      partestDependency(scalaVersion.value),
    )
  )

commands in Global ++= List(
  Commandz.partestCommand(baseDirectory.value),
  Commandz.replCommand,
)

lazy val root = project
  .in(file("."))
  .settings(publishArtifact := false, skip in publish := true)
  .aggregate(meta.js, meta.jvm, plugin, example.js, example.jvm, tests)
  .enablePlugins(ScalaJSPlugin)
