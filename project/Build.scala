import sbt._
import sbt.Keys._

object MyBuild extends Build {
  lazy val project = Project("root", file(".")) settings(
    name := "Scala Numerics",
    organization := "n/a",

    scalaVersion := "2.9.1",
    version := "0.1",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize"),

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.6.1",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://n0d.es/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    ),

    resolvers ++= Seq(
      "Scala-Tools" at "http://www.scala-tools.org/repo-reloases/",
      "Sonatype" at "http://oss.sonatype.org/content/repositories/snapshots"
    ),

    // caliper stuff stolen shamelessly from scala-benchmarking-template

    // enable forking in run
    fork in run := true,

    // custom kludge to get caliper to see the right classpath

    // define the onLoad hook
    onLoad in Global <<= (onLoad in Global) ?? identity[State],
    {
      // attribute key to prevent circular onLoad hook
      val key = AttributeKey[Boolean]("loaded")
      val f = (s: State) => {
        val loaded: Boolean = s get key getOrElse false
        if (!loaded) {
          var cpString: String = ""
          // get the runtime classpath
          Project.evaluateTask(fullClasspath.in(Runtime), s) match {
            // make a colon-delimited string of the classpath
            case Some(Value(cp)) => cpString = cp.files.mkString(":")
            // probably should handle an error here, but not sure you can
            //  ever get here with a working sbt
            case _ => Nil
          }
          val extracted: Extracted = Project.extract(s)
          // return a state with loaded = true and javaOptions set correctly
          extracted.append(Seq(javaOptions in run ++= Seq("-cp", cpString)), s.put(key, true))
        } else {
          // return the state, unmodified
          s
        }
      }
      onLoad in Global ~= (f compose _)
    }
  )
}
