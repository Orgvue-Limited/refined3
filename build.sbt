ThisBuild / scalaVersion := "3.5.0-RC4"
ThisBuild / organization := "com.orgvue"
ThisBuild / version      := "0.1.0"

lazy val core = (project in file("."))
  .settings(
    name := "core",
    scalacOptions ++= Seq(
      "-explain"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )

