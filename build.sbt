lazy val settings =
  Seq(
    organization := "com.orgvue",
    scalaVersion := "3.5.2",
    version := "0.1.0",
    scalacOptions := Seq("-explain")
  )

lazy val pureconfigDependency = "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.7"
lazy val scalaTestDependency  = "org.scalatest"         %% "scalatest"                 % "3.2.18"  % Test

lazy val core =
  project
    .settings(settings)
    .settings(moduleName := "refined3-core")
    .settings(libraryDependencies ++= Seq(scalaTestDependency))

lazy val pureconfig =
  project
    .settings(settings)
    .settings(moduleName := "refined3-pureconfig")
    .settings(libraryDependencies ++= Seq(pureconfigDependency, scalaTestDependency))
    .dependsOn(core)