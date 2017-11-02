version := "1.0"

scalaVersion := Settings.versions.scala

libraryDependencies := Seq(
    // ScalaTest
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value