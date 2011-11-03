name := "Scala Numerics Refactor"

organization := "n/a"

scalaVersion := "2.9.1"

resolvers ++= Seq("Scala Test" at "http://www.scala-tools.org/repo-reloases/")

libraryDependencies ++= {
  Seq("org.scalatest" %% "scalatest" % "1.6.1")
}

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-optimize"
