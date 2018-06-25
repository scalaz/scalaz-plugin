import Scalaz._

cancelable in Global := true

organization in ThisBuild := "org.scalaz"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
scalacOptions ++= List("-Xplugin-require:macroparadise")

lazy val meta = crossProject.module
  .settings(stdSettings("meta"))
lazy val metaJVM = meta.jvm
lazy val metaJS = meta.js

def partestOnly(in: String): Def.Initialize[Task[Unit]] =
  (testOnly in Test).toTask(" -- " + in)

def partestDesc(in: String): Def.Initialize[Task[(Result[Unit], String)]] =
  partestOnly(in).result map (_ -> s"partest $in")

lazy val testPending = taskKey[Unit]("Run all pending tests.")

lazy val plugin = (project in file("plugin"))
  .settings(
    crossVersion := CrossVersion.full,
    libraryDependencies ++= List(
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      partestDependency(scalaVersion.value) % Test))
  .settings(
    partestFramework,
    testPending := {
      partestDesc("--srcpath pending").value
    })
  .dependsOn(metaJVM)

lazy val PluginDependency: List[Def.Setting[_]] = List(
  scalacOptions ++= {
    val jar = (packageBin in Compile in plugin).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}",
        s"-Jdummy=${jar.lastModified}")
  })

lazy val example = crossProject.module
  .settings(stdSettings("example"))
  .settings(
    publishArtifact := false,
    skip in publish := true)
  .dependsOn(meta)
  .settings(PluginDependency:_*)

lazy val exampleJVM = example.jvm
lazy val exampleJS  = example.js

lazy val root = project.in(file("."))
  .settings(
    publishArtifact := false,
    skip in publish := true)
  .aggregate(metaJS, metaJVM, plugin, exampleJS, exampleJVM)
  .enablePlugins(ScalaJSPlugin)