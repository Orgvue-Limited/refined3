ThisBuild / scalaVersion := "3.5.0-RC3"
ThisBuild / organization := "com.refined"

lazy val core = (project in file("."))
  .settings(
    name := "core",
    scalacOptions ++= Seq(
      "-explain"
    )
  )

