import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "Spire",
    version := "0.2.0",
    scalaVersion := "2.9.1",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize"),
    resolvers ++= Seq(
      //"Scala-Tools" at "http://www.scala-tools.org/repo-reloases/",
      "Sonatype" at "http://oss.sonatype.org/content/repositories/snapshots"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.6.1"

    )


    /*
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
            println(">>> " + cpString)
          // return a state with loaded = true and javaOptions set correctly
          extracted.append(Seq(javaOptions in run ++= Seq("-cp", cpString)), s.put(key, true))
        } else {
          // return the state, unmodified
          s
        }
      }
      onLoad in Global ~= (f compose _)
    }
    */
  )

  val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val spire = Project("root", file("."))

  lazy val benchmark: Project = Project("benchmark", file("benchmark")) settings (benchmarkSettings: _*) dependsOn (spire)


  def benchmarkSettings = Seq(
    // raise memory limits here if necessary
    //javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.collections" % "google-collections" % "1.0",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      //"com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://n0d.es/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    ),

    // enable forking in run
    fork in run := true,

    // custom kludge to get caliper to see the right classpath

    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    // unfortunately `javaOptions` is a SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
    // jump through these hoops here in order to feed the result of the latter into the former
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime in benchmark, state).get._2.toEither.right.get.files.mkString(":")
            println(":-) " + classPath)
            // return a state with javaOptionsPatched = true and javaOptions set correctly
            Project.extract(state).append(Seq(javaOptions in (benchmark, run) ++= Seq("-cp", classPath)), state.put(key, true))
          case Some(_) =>
            state // the javaOptions are already patched
        }
      }
    }


    // caliper stuff stolen shamelessly from scala-benchmarking-template
  )
}
