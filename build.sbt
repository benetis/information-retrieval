import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "parsing-heaven",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"
  )
