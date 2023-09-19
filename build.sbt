name := "fpinscala"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

ThisBuild / libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
