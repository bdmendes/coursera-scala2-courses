course := "reactive"
assignment := "async"

testFrameworks += new TestFramework("munit.Framework")
Test / parallelExecution := false

scalaVersion := "2.13.12"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xlint",
)

libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test
