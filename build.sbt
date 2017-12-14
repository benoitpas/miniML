lazy val commonSettings = Seq(
  organization := "com.rawpack",
  version := "0.1.0",
  scalaVersion := "2.12.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "parser"
  )

libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

EclipseKeys.withSource := true
