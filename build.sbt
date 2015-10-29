name := """aula-virtual"""

version := "1.0-SNAPSHOT"

// requires java 1.8

scalaVersion := "2.11.1"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

mainClass in Compile := Some("SimpleExample")

// scalaSource in Compile := baseDirectory.value / "app" / "org"

libraryDependencies ++= Seq(
  jdbc,
  // anorm,
  cache,
  ws,
  "com.typesafe.play" %% "play-slick" % "1.0.1",
  // "com.typesafe.slick" %% "slick" % "2.1.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  // "com.h2database" % "h2" % "1.3.170",
  "mysql" % "mysql-connector-java" % "5.1.26",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0-M3"
