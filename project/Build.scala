import sbt._
import sbt.Keys._

import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

import com.typesafe.sbt.pgp.PgpKeys._

import pl.project13.scala.sbt.SbtJmh

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import sbtbuildinfo.Plugin._

object MyBuild extends Build {

  // Dependencies

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.1.3"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.3"

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

    scalaVersion := "2.11.2",

    crossScalaVersions := Seq("2.10.2", "2.11.2"),

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("http://spire-math.org")),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "machinist" % "0.3.0"
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
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",

    // re-enable to check imports, or once scala's REPL manages to not
    // be useless under -Ywarn-unused-import.

    // scalacOptions in Compile := {
    //   CrossVersion.partialVersion(scalaVersion.value) match {
    //     case Some((2, 10)) =>
    //       scalacOptions.value
    //     case Some((2, n)) if n >= 11 =>
    //       scalacOptions.value ++ Seq("-Ywarn-unused-import")
    //   }
    // },

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value

        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.0")
      }
    },

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
    aggregate(macros, core, examples, scalacheckBinding, tests, benchmark).
    settings(spireSettings: _*)

  lazy val spireSettings = Seq(
    name := "spire-aggregate"
  ) ++ noPublish ++ unidocSettings ++ Seq(
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(examples, benchmark, tests)
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
      scalaTest % "test"
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
      "com.chuusai" %% "shapeless" % "1.2.4",
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
      "org.typelevel" %% "discipline" % "0.2.1",
      scalaCheck
    )
  )

  // Tests

  lazy val tests = Project("tests", file("tests")).
    settings(testsSettings: _*).
    dependsOn(core, scalacheckBinding)

  lazy val testsSettings = Seq(
    name := "spire-tests",
    libraryDependencies ++= Seq(
      scalaTest % "test"
    )
  ) ++ noPublish


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

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
    fork in run := true
  ) ++ noPublish

  lazy val benchmarkJmh: Project = Project("benchmark-jmh", file("benchmark-jmh")).
    settings(benchmarkJmhSettings: _*).
    dependsOn(core, benchmark)

  lazy val benchmarkJmhSettings = SbtJmh.jmhSettings ++ Seq(
    name := "spire-benchmark-jmh"
  ) ++ noPublish

}
