import sbt._
import sbt.Keys._

import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

import com.typesafe.sbt.pgp.PgpKeys._

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import sbtbuildinfo.Plugin._

object MyBuild extends Build {

  // Dependencies

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.1"

  // Release step

  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val extracted = st.extract
      val ref = extracted.get(thisProjectRef)
      extracted.runAggregated(publishSigned in Global in ref, st)
    },
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
      st
    },
    enableCrossBuild = true
  )

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "org.spire-math",

    scalaVersion := "2.10.2",

    // disable annoying warnings about 2.10.x
    conflictWarning in ThisBuild := ConflictWarning.disable,

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("http://spire-math.org")),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.2"
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

    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.10.2" % "2.0.0-SNAPSHOT"),

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
    aggregate(macros, core, examples, scalacheckBinding, benchmark).
    settings(spireSettings: _*)

  lazy val spireSettings = Seq(
    name := "spire-aggregate"
  ) ++ noPublish ++ unidocSettings ++ Seq(
    excludedProjects in unidoc in ScalaUnidoc ++= Seq("examples", "benchmark")
  ) ++ releaseSettings ++ Seq(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishSignedArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

  // Macros

  lazy val macros = Project("macros", file("macros")).
    settings(macroSettings: _*)

  lazy val macroSettings = Seq(
    name := "spire-macros",
    libraryDependencies ++= Seq(scalaTest % "test", scalaCheck % "test")
  )

  // Core

  lazy val core = Project("core", file("core")).
    settings(coreSettings: _*).
    dependsOn(macros)

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
    libraryDependencies ++= Seq(
      scalaCheck % "test",
      scalaTest % "test",
      "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"
    )
  ) ++ buildInfoSettings ++ Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := "spire"
  )

  // Examples

  lazy val examples = Project("examples", file("examples")).
    settings(examplesSettings: _*).
    dependsOn(core)

  lazy val examplesSettings = Seq(
    name := "spire-examples",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "1.2.3",
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1"
    )
  ) ++ noPublish

  // Scalacheck binding

  lazy val scalacheckBinding = Project("scalacheck-binding", file("scalacheck-binding")).
    settings(scalacheckSettings: _*).
    dependsOn(core)

  lazy val scalacheckSettings = Seq(
    name := "spire-scalacheck-binding",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "discipline" % "0.1",
      scalaTest % "test",
      scalaCheck
    )
  )


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

  lazy val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val benchmarkSettings = Seq(
    name := "spire-benchmark",

    // raise memory limits here if necessary
    // TODO: this doesn't seem to be working with caliper at the moment :(
  
    javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
      // comparisons
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1",
      "org.apache.commons" % "commons-math3" % "3.2",

      // thyme
      "ichi.bench" % "thyme" % "0.1.0" from "http://plastic-idolatry.com/jars/thyme-0.1.0.jar",

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
  ) ++ noPublish

}
