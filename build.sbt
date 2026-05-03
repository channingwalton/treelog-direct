ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization  := "com.casualmiracles"
ThisBuild / version       := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "treelog-direct",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test
  )
