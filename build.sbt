ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(name := "aoc2023")
  .settings(
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.7.3"
    )
  )