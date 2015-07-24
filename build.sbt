import sbtbuildinfo.Plugin._
import sbtunidoc.{Plugin => UnidocPlugin}
import sbtunidoc.Plugin.UnidocKeys._
//import com.typesafe.sbt.pgp.PgpKeys._
import pl.project13.scala.sbt.SbtJmh
import ReleaseTransformations._

// Projects

lazy val spire = project.in(file("."))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .aggregate(macros, core, examples, laws, tests, benchmark)
  .dependsOn(macros, core, examples, laws, tests, benchmark)

lazy val macros = project
  .settings(moduleName := "spire-macros")
  .settings(spireSettings)
  .settings(macroSettings)

lazy val core = project
  .settings(moduleName := "spire")
  .settings(spireSettings)
  .settings(coreSettings)
  .dependsOn(macros)

lazy val examples = project
  .settings(moduleName := "spire-examples")
  .settings(spireSettings)
  .settings(examplesSettings)
  .settings(noPublishSettings)
  .dependsOn(core)

lazy val laws = project.in(file("laws"))
  .settings(moduleName := "spire-laws")
  .settings(spireSettings)
  .settings(scalacheckSettings)
  .dependsOn(core)

lazy val tests = project
  .settings(moduleName := "spire-tests")
  .settings(spireSettings)
  .settings(testsSettings)
  .settings(noPublishSettings)
  .dependsOn(core, laws)

lazy val benchmark = project
  .settings(moduleName := "spire-benchmark")
  .settings(spireSettings)
  .settings(benchmarkSettings)
  .settings(noPublishSettings)
  .dependsOn(core)

lazy val benchmarkJmh: Project = project.in(file("benchmark-jmh"))
  .settings(moduleName := "spire-benchmark-jmh")
  .settings(spireSettings)
  .settings(SbtJmh.jmhSettings)
  .settings(noPublishSettings)
  .dependsOn(core, benchmark)


// General settings

lazy val buildSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.11.7",
  // https://github.com/non/spire/pull/413#issuecomment-89896773
  crossScalaVersions := Seq("2.10.4", "2.11.7")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Yinline-warnings",
    "-deprecation",
    "-encoding", "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    //"-Xfatal-warnings", // inliner warnings mean we leave this off
    "-Xlint",
    "-Xfuture",
    "-Yno-adapted-args",
    "-optimize",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.typelevel" %% "machinist" % "0.3.0" // TODO: 0.3.1 causes compilation errors
  ) ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        Seq()

      // in Scala 2.10, quasiquotes are provided by macro-paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
          "org.scalamacros" %% "quasiquotes" % "2.0.1"
        )
    }
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://spire-math.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),

  publishTo <<= (version).apply { v =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
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
  ),

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges))

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false)

addCommandAlias("validate", ";core/scalastyle;compile;test")

// Dependencies

lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.2"


// Project's settings

lazy val spireSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val unidocSettings = UnidocPlugin.unidocSettings ++ Seq(
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(examples, benchmark, tests)
)

lazy val macroSettings = Seq(
  libraryDependencies ++= Seq(scalaTest % "test", scalaCheck % "test"),
  unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala_${scalaBinaryVersion.value}"
)

lazy val genProductTypes = TaskKey[Seq[File]]("gen-product-types", "Generates several type classes for Tuple2-22.")

lazy val coreSettings = Seq(
  sourceGenerators in Compile <+= (genProductTypes in Compile),
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
  ),
  unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala_${scalaBinaryVersion.value}"
) ++ buildInfoSettings ++ Seq(
  sourceGenerators in Compile <+= buildInfo,
  buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
  buildInfoPackage := "spire"
)

lazy val examplesSettings = Seq(
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "1.2.4",
    "org.apfloat" % "apfloat" % "1.8.2",
    "org.jscience" % "jscience" % "4.3.1"
  )
)

lazy val scalacheckSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "discipline" % "0.2.1",
    scalaCheck
  )
)

lazy val testsSettings = Seq(
  libraryDependencies ++= Seq(
    scalaTest % "test"
  )
)

lazy val benchmarkSettings = Seq(
  // raise memory limits here if necessary
  // TODO: this doesn't seem to be working with caliper at the moment :(
  javaOptions in run += "-Xmx4G",

  libraryDependencies ++= Seq(
    // comparisons
    "org.apfloat" % "apfloat" % "1.6.3",
    "org.jscience" % "jscience" % "4.3.1",
    "org.apache.commons" % "commons-math3" % "3.4.1",

    // thyme
    "ichi.bench" % "thyme" % "0.1.0" from "http://plastic-idolatry.com/jars/thyme-0.1.0.jar",

    // caliper stuff
    "com.google.guava" % "guava" % "18.0",
    "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.1",
    "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
    "com.google.code.gson" % "gson" % "1.7.2"
  ),

  // enable forking in run
  fork in run := true
)
