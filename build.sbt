name := "scala-functional-library"

version := "0.1"

scalaVersion := "2.13.5"

lazy val scalaTestVersion = "3.2.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
