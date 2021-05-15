import Dependencies._

ThisBuild / scalaVersion     := "3.0.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"


lazy val root = (project in file("."))
  .settings(
    name := "mushikitty",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "cats-laws" % "2.6.1",
      "org.scalameta" %% "munit-scalacheck" % "0.7.26",
      "org.typelevel" %% "discipline-munit" % "1.0.9",
      "org.scalameta" %% "munit" % "0.7.26"
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
