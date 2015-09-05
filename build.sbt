lazy val buildSettings = List(
  organization := "com.adelbertc",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalaVersion := "2.10.5",
  version := "0.1.0-SNAPSHOT"
)

lazy val commonSettings = List(
  scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ),
  libraryDependencies +=
    compilerPlugin("org.scalamacros"  % ("paradise_" ++ scalaVersion.value) % scalamacrosVersion),
  initialize := { System.setProperty("tfm.verbose", "") }
)

lazy val tfmSettings = buildSettings ++ commonSettings

lazy val tfm =
  project.in(file(".")).
  settings(tfmSettings ++ List(run <<= run.in(Compile).in(core))).
  aggregate(core, examples)

val scalamacrosVersion = "2.0.1"

lazy val core =
  project.in(file("core")).
  settings(name := "tfm").
  settings(description := "Annotation macro to generate EDSL code in the tagless final style").
  settings(tfmSettings).
  settings(
    libraryDependencies ++= List(
      "org.scalamacros" %% "quasiquotes"   % scalamacrosVersion,
      "org.scala-lang"  %  "scala-reflect" % scalaVersion.value
    )
  )

lazy val examples =
  project.in(file("examples")).
  settings(tfmSettings).
  dependsOn(core)
