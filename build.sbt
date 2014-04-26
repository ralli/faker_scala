name := "faker_scala"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.13",
  "org.scalatest" %% "scalatest" % "2.1.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)
