organization := "org.faker"

publishMavenStyle := true

name := "faker_scala"

version := "0.9.2-SNAPSHOT"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-feature", "-deprecation")

publishArtifact in Test := false

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.13",
  "org.scalatest" %% "scalatest" % "2.1.6" % "test"
)

pomExtra :=
  <url>https://github.com/ralli/faker_scala</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:ralli/faker_scala.git</url>
      <connection>scm:git:git@github.com:ralli/faker_scala.git</connection>
    </scm>
    <developers>
      <developer>
        <id>ralli</id>
        <name>Ralph Juhnke</name>
        <url>https://github.com/ralli</url>
      </developer>
    </developers>
