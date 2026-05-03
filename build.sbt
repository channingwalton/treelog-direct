ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization  := "org.channingwalton"
ThisBuild / version       := "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"             % "1.0.4" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(core, cats)
  .settings(
    name           := "treelog-direct-root",
    publish / skip := true
  )

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "treelog-direct-core"
  )

lazy val cats = (project in file("cats"))
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "treelog-direct-cats",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
  )
