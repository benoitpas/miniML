lazy val commonSettings = Seq(
  organization := "com.rawpack",
  version := "0.1.0",
  scalaVersion := "2.13.12"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "parser"
  )

libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "org.scalatestplus" %% "scalatestplus-junit" % "1.0.0-M2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

scalacOptions ++= Seq(/*"-Xmigration",*/"-deprecation")

EclipseKeys.withSource := true
