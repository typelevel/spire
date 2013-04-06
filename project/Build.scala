import sbt._
import sbt.Keys._

object MyBuild extends Build {

  // Dependencies

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "1.9.1"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.0"

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "org.spire-math",
    version := "0.4.0-M4",

    scalaVersion := "2.10.0",

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("http://spire-math.org")),

    libraryDependencies ++= Seq(
      scalaTest % "test",
      "org.scala-lang" % "scala-reflect" % "2.10.0"
    ),

    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      "-Yinline-warnings",
      "-deprecation",
      "-unchecked",
      "-optimize",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    ),

    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },

    publishTo <<= version {
      (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) 
        Some("snapshots" at nexus + "content/repositories/snapshots") 
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

    pomExtra := (
<scm>
  <url>git@github.com:non/spire.git</url>
  <connection>scm:git:git@github.com:non/spire.git</connection>
</scm>
<developers>
  <developer>
    <id>d_m</id>
    <name>Erik Osheim</name>
    <url>http://github.com/non/</url>
  </developer>
  <developer>
    <id>tixxit</id>
    <name>Tom Switzer</name>
    <url>http://github.com/tixxit/</url>
  </developer>
</developers>

    )
  )

  // Main

  lazy val spire = Project("spire", file(".")).
    aggregate(core, examples, scalacheckBinding).
    settings(spireSettings: _*)

  lazy val spireSettings = Seq(
    name := "spire-aggregate",
    publish := false,
    publishLocal := false
  )

  // Core project

  lazy val core = Project("core", file("core")).
    settings(coreSettings: _*)

  lazy val genProductTypes = TaskKey[Seq[File]]("gen-product-types",
    "Generates several type classes for Tuple2-22.")

  lazy val coreSettings = Seq(
    name := "spire",
    sourceGenerators in Compile <+= (genProductTypes in Compile).task,
    genProductTypes <<= (sourceManaged in Compile, streams) map { (scalaSource, s) =>
      s.log.info("Generating spire/std/tuples.scala")
      val algebraSource = ProductTypes.algebraProductTypes
      val algebraFile = (scalaSource / "spire" / "std" / "tuples.scala").asFile
      IO.write(algebraFile, algebraSource)

      Seq[File](algebraFile)
    },
    libraryDependencies += scalaCheck % "test"
  )

  // Examples

  lazy val examples = Project("examples", file("examples")).
    settings(examplesSettings: _*).
    dependsOn(core)

  lazy val examplesSettings = Seq(
    //scalacOptions ++= Seq("-Ymacro-debug-lite"),
    publish := false,
    publishLocal := false,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "1.2.3",
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1"
    )
  )

  // Scalacheck binding

  lazy val scalacheckBinding = Project("scalacheck-binding", file("scalacheck-binding")).
    settings(scalacheckSettings: _*).
    dependsOn(core)

  lazy val scalacheckSettings = Seq(
    name := "spire-scalacheck-binding",
    libraryDependencies ++= Seq(scalaTest, scalaCheck)
  )


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

  lazy val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val benchmarkSettings = Seq(
    // raise memory limits here if necessary
    // TODO: this doesn't seem to be working with caliper at the moment :(
  
    javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
      // comparisons
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1",

      // caliper stuff
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    ),

    // enable forking in run
    fork in run := true,

    // custom kludge to get caliper to see the right classpath

    // we need to add the runtime classpath as a "-cp" argument to the
    // `javaOptions in run`, otherwise caliper will not see the right classpath
    // and die with a ConfigurationException unfortunately `javaOptions` is a
    // SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
    // jump through these hoops here in order to feed the result of the latter
    // into the former
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime in benchmark, state).get._2.toEither.right.get.files.mkString(":")
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
