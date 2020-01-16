name := "tst-rate-service"

version := "1.0"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)


mainClass in (Compile, run) := Some("com.tst.Main")