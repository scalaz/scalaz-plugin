import Scalaz._

cancelable in Global := true

organization in ThisBuild := "org.scalaz"

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

dynverSonatypeSnapshots in ThisBuild := true

lazy val sonataCredentials = for {
  username <- sys.env.get("SONATYPE_USERNAME")
  password <- sys.env.get("SONATYPE_PASSWORD")
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)

credentials in ThisBuild ++= sonataCredentials.toSeq

licenses in ThisBuild += ("LGPL-3.0", url("https://opensource.org/licenses/LGPL-3.0"))

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
scalacOptions ++= List("-Xplugin-require:macroparadise")

lazy val meta = crossProject.module
  .settings(stdSettings("plugin-library"))
lazy val metaJVM = meta.jvm
lazy val metaJS  = meta.js

lazy val plugin = (project in file("plugin"))
  .settings(
    name := "scalaz-plugin",
    crossVersion := CrossVersion.full,
    libraryDependencies ++= List(
      scalaOrganization.value % "scala-reflect"  % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
    )
  )
  .dependsOn(metaJVM)

lazy val PluginDependency: List[Def.Setting[_]] = List(scalacOptions ++= {
  val jar = (packageBin in Compile in plugin).value
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
})

lazy val example = crossProject.module
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

commands in Global += Commandz.partestCommand(baseDirectory.value)

lazy val exampleJVM = example.jvm
lazy val exampleJS  = example.js

lazy val root = project
  .in(file("."))
  .settings(publishArtifact := false, skip in publish := true)
  .aggregate(metaJS, metaJVM, plugin, exampleJS, exampleJVM, tests)
  .enablePlugins(ScalaJSPlugin)
