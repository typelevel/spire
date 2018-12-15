import microsites._
import ReleaseTransformations._

import sbtcrossproject.{CrossType, crossProject}

lazy val scalaVersions: Map[String, String] =
  Map("2.11" -> "2.11.12", "2.12" -> "2.12.6")

lazy val scalaCheckVersion = "1.14.0"
lazy val scalaTestVersion = "3.0.6-SNAP1"
lazy val shapelessVersion = "2.3.3"
lazy val disciplineVersion = "0.10.0"
lazy val machinistVersion = "0.6.4"
lazy val algebraVersion = "1.0.0"

lazy val apfloatVersion = "1.8.2"
lazy val jscienceVersion = "4.3.1"
lazy val apacheCommonsMath3Version = "3.4.1"


// Projects

lazy val spire = project.in(file("."))
  .settings(moduleName := "spire-root")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(spireJVM, spireJS)
  .dependsOn(spireJVM, spireJS)

lazy val spireJVM = project.in(file(".spireJVM"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(macrosJVM, coreJVM, dataJVM, extrasJVM, examples, lawsJVM, legacyJVM, platformJVM, testsJVM, utilJVM, benchmark)
  .dependsOn(macrosJVM, coreJVM, dataJVM, extrasJVM, examples, lawsJVM, legacyJVM, platformJVM, testsJVM, utilJVM, benchmark)

lazy val spireJS = project.in(file(".spireJS"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(macrosJS, coreJS, dataJS, extrasJS, lawsJS, legacyJS, platformJS, testsJS, utilJS)
  .dependsOn(macrosJS, coreJS, dataJS, extrasJS, lawsJS, legacyJS, platformJS, testsJS, utilJS)
  .enablePlugins(ScalaJSPlugin)

lazy val platform = crossProject(JSPlatform, JVMPlatform)
  .settings(moduleName := "spire-platform")
  .settings(spireSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(macros, util)

lazy val platformJVM = platform.jvm
lazy val platformJS = platform.js

lazy val macros = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-macros")
  .settings(spireSettings:_*)
  .settings(scalaCheckSettings:_*)
  .settings(scalaTestSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

lazy val data = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-data")
  .settings(spireSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)

lazy val dataJVM = data.jvm
lazy val dataJS = data.js

lazy val legacy = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-legacy")
  .settings(spireSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)

lazy val legacyJVM = legacy.jvm
lazy val legacyJS = legacy.js

lazy val util = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-util")
  .settings(spireSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(macros)

lazy val utilJVM = util.jvm
lazy val utilJS = util.js

lazy val core = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire")
  .settings(spireSettings:_*)
  .settings(coreSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(macros, platform, util)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val extras = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-extras")
  .settings(spireSettings:_*)
  .settings(extrasSettings:_*)
  .settings(crossVersionSharedSources:_*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .dependsOn(macros, platform, util, core, data)

lazy val extrasJVM = extras.jvm
lazy val extrasJS = extras.js

lazy val docs = project.in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .dependsOn(macrosJVM, coreJVM, extrasJVM)
  .settings(moduleName := "spire-docs")
  .settings(spireSettings:_*)
  .settings(docSettings: _*)
  .settings(noPublishSettings)
  .enablePlugins(TutPlugin)
  .settings(commonJvmSettings:_*)

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

lazy val laws = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
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

lazy val tests = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "spire-tests")
  .settings(spireSettings:_*)
  .settings(scalaTestSettings:_*)
  .settings(noPublishSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(testOptions in Test := Seq(Tests.Filter(s => jsTests.contains(s))))
  .jsSettings(commonJsSettings:_*)
  .dependsOn(core, data, legacy, extras, laws)

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
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM, extrasJVM, benchmark)

// General settings

addCommandAlias("validateJVM", ";coreJVM/scalastyle;macrosJVM/test;coreJVM/test;extrasJVM/test;lawsJVM/test;testsJVM/test;examples/test;benchmark/test")

addCommandAlias("validateJS", ";macrosJS/test;coreJS/test;extrasJS/test;lawsJS/test;testsJS/test")

addCommandAlias("validate", ";validateJVM;validateJS")

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := scalaVersions("2.12"),
  crossScalaVersions := Seq(scalaVersions("2.11"), scalaVersions("2.12"))
)

lazy val commonDeps = Seq(libraryDependencies ++= Seq(
  "org.typelevel" %%% "machinist" % machinistVersion,
  "org.typelevel" %%% "algebra" % algebraVersion))

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.value.diff(Seq(
    "-Xfatal-warnings",
    "-language:existentials",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )),
  resolvers += Resolver.sonatypeRepo("snapshots")
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

// TODO: restore for UniDoc
// lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "Spire",
  micrositeDescription := "Powerful new number types and numeric abstractions for Scala",
  micrositeAuthor := "Spire contributors",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "http://denisrosset.github.io/spire",
  micrositeBaseUrl := "spire",
//  micrositeDocumentationUrl := "/symdpoly/api/net/alasc/symdpoly/index.html",
//  micrositeDocumentationLabelDescription := "API Documentation",
  micrositeGithubOwner := "denisrosset",
  micrositeGithubRepo := "spire",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"),
  micrositeConfigYaml := ConfigYml(
    yamlCustomProperties = Map(
      "spireVersion"    -> version.value,
      "scalaVersion"  -> scalaVersion.value
    )
  ),
//  autoAPIMappings := true,
//  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
//    inProjects(docsSourcesAndProjects(scalaVersion.value)._2:_*),
//  docsMappingsAPIDir := "api",
//  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork := true,
  javaOptions += "-Xmx4G", // to have enough memory in forks
//  fork in tut := true,
//  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-groups",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  scalacOptions in Tut ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))),
  git.remoteRepo := "git@github.com:denisrosset/spire.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value
)


lazy val publishSettings = Seq(
  homepage := Some(url("http://spire-math.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  pomExtra := (
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
  coverageHighlighting := true,
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
    "org.apache.commons" % "commons-math3" % apacheCommonsMath3Version
  ),

  // enable forking in run
  fork in run := true
)

lazy val coreSettings = Seq(
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

lazy val scalaCheckSettings  = Seq(libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test")

lazy val scalaTestSettings = Seq(
  libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion % "test",
  libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion % "test"
)

lazy val spireSettings = buildSettings ++ commonSettings ++ commonDeps ++ publishSettings ++ scoverageSettings

lazy val unidocSettings = Seq(
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(examples, benchmark, testsJVM)
)

////////////////////////////////////////////////////////////////////////////////////////////////////
// Base Build Settings - Should not need to edit below this line.
// These settings could also come from another file or a plugin.
// The only issue if coming from a plugin is that the Macro lib versions
// are hard coded, so an overided facility would be required.

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
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
  libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"
)

lazy val commonScalacOptions = Def.setting(
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v >= 13 =>
      Seq()
    case _ =>
      Seq("-Yno-adapted-args")
  }) ++ Seq(
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
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  )
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
    releaseStepCommand("sonatypeReleaseAll"),
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
