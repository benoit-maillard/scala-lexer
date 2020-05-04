lazy val sl = (project in file(".")) 
  .settings(
    name := "scala-lexer",

    version := "1.6",
    organization := "ch.epfl.lara",
    scalaVersion := "2.12.10",

    scalacOptions ++= Seq("-feature"),

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
  )
