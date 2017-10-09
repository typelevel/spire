import sbtbuildinfo.Plugin._
import sbtunidoc.{Plugin => UnidocPlugin}
import sbtunidoc.Plugin.UnidocKeys._
import pl.project13.scala.sbt.SbtJmh
import ReleaseTransformations._

lazy val scalaVersions: Map[String, String] = Map("2.10" -> "2.10.6", "2.11" -> "2.11.8", "2.12" -> "2.12.1")

lazy val scalaCheckVersion = "1.13.4"
lazy val scalaTestVersion = "3.0.0"
lazy val shapelessVersion = "2.3.2"
lazy val disciplineVersion = "0.7.2"
lazy val machinistVersion = "0.6.1"
lazy val algebraVersion = "0.7.0"
// lazy val catsVersion = "0.8.1"

lazy val apfloatVersion = "1.8.2"
lazy val jscienceVersion = "4.3.1"
lazy val apacheCommonsMath3Version = "3.4.1"


// Projects

lazy val spire = project.in(file("."))
  .settings(moduleName := "spire-root")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .aggregate(spireJVM, spireJS)
  .dependsOn(spireJVM, spireJS)

lazy val spireJVM = project.in(file(".spireJVM"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .aggregate(kernelJVM, macrosJVM, coreJVM, extrasJVM, examples, lawsJVM, testsJVM, benchmark)
  .dependsOn(kernelJVM, macrosJVM, coreJVM, extrasJVM, examples, lawsJVM, testsJVM, benchmark)

lazy val spireJS = project.in(file(".spireJS"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .aggregate(kernelJS, macrosJS, coreJS, extrasJS, lawsJS, testsJS)
  .dependsOn(kernelJS, macrosJS, coreJS, extrasJS, lawsJS, testsJS)
  .enablePlugins(ScalaJSPlugin)

lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "spire-macros")
  .settings(spireSettings:_*)
  .settings(scalaCheckSettings:_*)
  .settings(scalaTestSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(kernel)

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

lazy val core = crossProject
  .settings(moduleName := "spire")
  .settings(spireSettings:_*)
  .settings(coreSettings:_*)
  .settings(buildInfoSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(kernel, macros)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val kernel =  crossProject.crossType(CrossType.Full)
  .settings(moduleName := "spire-kernel")
  .settings(spireSettings: _*)
  .settings(scalaCheckSettings:_*)
  .settings(scalaTestSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)

lazy val kernelJVM = kernel.jvm
lazy val kernelJS = kernel.js

lazy val extras = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "spire-extras")
  .settings(spireSettings:_*)
  .settings(extrasSettings:_*)
  .settings(buildInfoSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(core)

lazy val extrasJVM = extras.jvm
lazy val extrasJS = extras.js

lazy val examples = project
  .settings(moduleName := "spire-examples")
  .settings(spireSettings)
  .settings(libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "org.apfloat" % "apfloat" % apfloatVersion,
    "org.jscience" % "jscience" % jscienceVersion
  ))
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, extrasJVM)

lazy val laws = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "spire-laws")
  .settings(spireSettings:_*)
  .settings(libraryDependencies ++= Seq(
    "org.typelevel" %%% "discipline" % disciplineVersion,
    "org.scalacheck" %%% "scalacheck" % scalaCheckVersion
  ))
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(core, extras)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

// Todo: As all tests in this list are commented out, no tests in testJS are run - but they are compiled.
//       This list is TEMPORARY as tests are migrated to scala-js
lazy val jsTests = List(
 /* "spire.PartialOrderSyntaxTest",
  "spire.PartialSyntaxTest",
  "spire.SyntaxTest",
  //"spire.algebra.GCDTest",
  "spire.algebra.NRootTest",
  "spire.algebra.PartialOrderTest",
  "spire.algebra.RingTest","*/
  "spire.algebra.SignedTest"/*,
  "spire.algebra.TrigTest",
  "spire.laws.LawTests",
  "spire.math.AlgebraicTest",
  "spire.math.BinaryMergeCheck",
  "spire.math.BitStringCheck",
  "spire.math.BitStringTest",
  "spire.math.ComplexCheck",
  "spire.math.ComplexCheck2",
  "spire.math.ComplexTest",
  "spire.math.ContinuousIntervalTest",
  "spire.math.CooperativeEqualityTest",
  "spire.math.FastComplexCheck",
  "spire.math.FixedPointCheck",
  "spire.math.FpFilterTest",
  "spire.math.IntervalCheck",
  "spire.math.IntervalGeometricPartialOrderTest",
  "spire.math.IntervalIteratorCheck",
  "spire.math.IntervalReciprocalTest",
  "spire.math.IntervalSubsetPartialOrderTest",
  "spire.math.IntervalTest",
  "spire.math.JetTest",
  "spire.math.LinearSelectTest",
  "spire.math.LiteralsTest",
  "spire.math.MergingTest",
  "spire.math.NaturalTest",
  "spire.math.NumberPropertiesTest",
  "spire.math.NumberTest",
  "spire.math.NumericTest",
  "spire.math.PackageCheck",
  "spire.math.PackageTest",
  "spire.math.PolynomialCheck",
  "spire.math.PolynomialSamplingCheck",
  "spire.math.PolynomialTest",
  "spire.math.QuaternionCheck",
  "spire.math.QuickSelectTest",
  "spire.math.RationalCheck",
  "spire.math.RationalTest",
  "spire.math.RealCheck",
  "spire.math.RingIntervalTest",
  "spire.math.SafeLongTest",
  "spire.math.SearchTest",
  "spire.math.SortingTest",
  "spire.math.TrileanCheck",
  "spire.math.UByteTest",
  "spire.math.UIntTest",
  "spire.math.ULongTest",
  "spire.math.UShortTest",
  "spire.math.prime.FactorHeapCheck",
  "spire.math.prime.FactorsCheck",
  "spire.random.GaussianTest",      
  "spire.random.GeneratorTest",
  "spire.random.SamplingTest",
  "spire.random.ShufflingTest",
  "spire.syntax.CforTest",
  "spire.util.OptCheck",
  "spire.util.PackCheck",
  "test.scala.spire.math.TypeclassExistenceTest"
*/
)

lazy val tests = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "spire-tests")
  .settings(spireSettings:_*)
  .settings(scalaTestSettings:_*)
  .settings(noPublishSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(testOptions in Test := Seq(Tests.Filter(s => jsTests.contains(s))))
  .jsSettings(commonJsSettings:_*)
  .dependsOn(core, extras, laws)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val benchmark = project
  .settings(moduleName := "spire-benchmark")
  .settings(spireSettings)
  .settings(benchmarkSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, extrasJVM)

lazy val benchmarkJmh: Project = project.in(file("benchmark-jmh"))
  .settings(moduleName := "spire-benchmark-jmh")
  .settings(spireSettings)
  .settings(SbtJmh.jmhSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, extrasJVM, benchmark)

// General settings

addCommandAlias("validateJVM", ";coreJVM/scalastyle;macrosJVM/test;coreJVM/test;extrasJVM/test;lawsJVM/test;testsJVM/test;examples/test;benchmark/test")

addCommandAlias("validateJS", ";macrosJS/test;coreJS/test;extrasJS/test;lawsJS/test;testsJS/test")

addCommandAlias("validate", ";validateJVM;validateJS")

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := scalaVersions("2.12"),
  crossScalaVersions := Seq(scalaVersions("2.10"), scalaVersions("2.11"), scalaVersions("2.12"))
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-language:existentials",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "machinist" % machinistVersion,
    "org.typelevel" %%% "algebra" % algebraVersion
  ),
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("snapshots")
  )
) ++ scalaMacroDependencies ++ warnUnusedImport

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  // -optimize has no effect in scala-js other than slowing down the build
  //  scalacOptions += "-optimize", // disabling for now
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor <= 11 => Seq("-optimize")
    case _ => Seq.empty
  }),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://spire-math.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
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
) ++ credentialSettings ++ sharedPublishSettings ++ sharedReleaseProcess

lazy val scoverageSettings = Seq(
  coverageMinimum := 40,
  coverageFailOnMinimum := false,
  coverageHighlighting := scalaBinaryVersion.value != "2.10",
  coverageExcludedPackages := "spire\\.benchmark\\..*;spire\\.macros\\..*"
)

// Project's settings

lazy val benchmarkSettings = Seq(
  // raise memory limits here if necessary
  // TODO: this doesn't seem to be working with caliper at the moment :(
  javaOptions in run += "-Xmx4G",

  libraryDependencies ++= Seq(
    // comparisons
    "org.apfloat" % "apfloat" % apfloatVersion,
    "org.jscience" % "jscience" % jscienceVersion,
    "org.apache.commons" % "commons-math3" % apacheCommonsMath3Version,

    // thyme
    "ichi.bench" % "thyme" %  "0.1.0" from "http://plastic-idolatry.com/jars/thyme-0.1.0.jar",

    // caliper stuff
    "com.google.guava" % "guava" %  "18.0",
    "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" %  "2.1",
    "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
    "com.google.code.gson" % "gson" % "1.7.2"
  ),

  // enable forking in run
  fork in run := true
)

lazy val coreSettings = Seq(
  sourceGenerators in Compile += buildInfo.taskValue,
  buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
  buildInfoPackage := "spire",
  sourceGenerators in Compile += (genProductTypes in Compile).taskValue,
  genProductTypes := {
    val scalaSource = (sourceManaged in Compile).value
    val s = streams.value
    s.log.info("Generating spire/std/tuples.scala")
    val algebraSource = ProductTypes.algebraProductTypes
    val algebraFile = (scalaSource / "spire" / "std" / "tuples.scala").asFile
    IO.write(algebraFile, algebraSource)

    Seq[File](algebraFile)
  }
)

lazy val extrasSettings = Seq(
//  sourceGenerators in Compile <+= buildInfo,
//  buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
//  buildInfoPackage := "spire.extras"
)

lazy val genProductTypes = TaskKey[Seq[File]]("gen-product-types", "Generates several type classes for Tuple2-22.")

lazy val scalaCheckSettings  = Seq(libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test")

lazy val scalaTestSettings = Seq(
  libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion % "test",
  libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion % "test"
)

lazy val spireSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val unidocSettings = UnidocPlugin.unidocSettings ++ Seq(
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(examples, benchmark, testsJVM)
)

////////////////////////////////////////////////////////////////////////////////////////////////////
// Base Build Settings - Should not need to edit below this line.
// These settings could also come from another file or a plugin.
// The only issue if coming from a plugin is that the Macro lib versions
// are hard coded, so an overided facility would be required.

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File =>
          CrossVersion.partialVersion(scalaBinaryVersion.value) match {
            case Some((major, minor)) =>
              new File(s"${dir.getPath}_$major.$minor")
            case None =>
              sys.error("couldn't parse scalaBinaryVersion ${scalaBinaryVersion.value}")
          }
      }
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.patch),
              "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
        )
    }
  }
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val sharedPublishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  }
)
 
lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges)
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
lazy val credentialSettings = Seq(
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)
