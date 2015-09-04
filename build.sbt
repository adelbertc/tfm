name := "tfm"

organization := "com.adelbertc"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.5"

val scalamacrosVersion = "2.0.1"

libraryDependencies ++= List(
  compilerPlugin("org.scalamacros"  % ("paradise_" ++ scalaVersion.value) % scalamacrosVersion),
  "org.scalamacros" %% "quasiquotes"   % scalamacrosVersion,
  "org.scala-lang"  %  "scala-reflect" % scalaVersion.value
)

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
)

