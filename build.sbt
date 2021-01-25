import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"


addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    name := "mushikitty",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.3.1",
      "org.typelevel" %% "cats-laws" % "2.3.1",
      "org.scalameta" %% "munit-scalacheck" % "0.7.21",
      "org.typelevel" %% "discipline-munit" % "1.0.4",
      "org.scalameta" %% "munit" % "0.7.21"
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
