organization := "org.faker"

publishTo := Some("Artifactory Realm" at "http://localhost:8081/artifactory/libs-release-local")

credentials += Credentials("Artifactory Realm", "localhost", "ralli", "schwobb-")



publishMavenStyle := true

name := "faker_scala"

version := "0.9.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-feature", "-deprecation")

publishArtifact in Test := false

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.13",
  "org.scalatest" %% "scalatest" % "2.1.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
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