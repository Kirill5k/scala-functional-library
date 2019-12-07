name := "scala-functional-library"

version := "0.1"

scalaVersion := "2.13.1"

lazy val scalaTestVersion = "3.0.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
