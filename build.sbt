lazy val buildSettings = List(
  organization := "com.adelbertc",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalaVersion := "2.11.7",
  crossScalaVersions := List("2.10.5", scalaVersion.value),
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
    "-Yrangepos",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies +=
    compilerPlugin("org.scalamacros"  % "paradise" % "2.0.1" cross CrossVersion.full),
  initialize := { System.setProperty("tfm.verbose", "") }
)

lazy val tfmSettings = buildSettings ++ commonSettings

lazy val tfm =
  project.in(file(".")).
  settings(tfmSettings).
  aggregate(core, examples)

lazy val core =
  project.in(file("core")).
  settings(name := "tfm").
  settings(description := "Annotation macro to generate EDSL code in the tagless final style").
  settings(tfmSettings).
  settings(
    libraryDependencies ++= List(
      "org.typelevel"   %% "macro-compat"   % "1.0.1",
      "org.scala-lang"  %  "scala-reflect"  % scalaVersion.value,
      "org.specs2"      %% "specs2-core"    % "3.6.4"             % "test"
    )
  )

lazy val examples =
  project.in(file("examples")).
  settings(tfmSettings).
  settings(libraryDependencies += "org.spire-math" %% "cats" % "0.2.0").
  dependsOn(core)
