
val header = """|**********************************************************************\
                |* Project                                                              **
                |*       ______  ______   __    ______    ____                          **
                |*      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
                |*     / /__   / /_/ /  / /   / /_/ /   / /_                            **
                |*    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
                |*   ____/ / / /      / /   / / | |   / /__                             **
                |*  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
                |*                                                                      **
                |*      Redistribution and use permitted under the MIT license.         **
                |*                                                                      **
                |\***********************************************************************
                |""".stripMargin

import scala.language.existentials
import microsites._

lazy val scalaCheckVersion = "1.15.4"

lazy val munit = "0.7.29"
lazy val munitDiscipline = "1.0.9"

lazy val algebraVersion = "2.2.3"

lazy val apfloatVersion = "1.10.1"
lazy val jscienceVersion = "4.3.1"
lazy val apacheCommonsMath3Version = "3.6.1"

val Scala213 = "2.13.6"
val Scala3 = "3.1.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / crossScalaVersions := Seq(Scala213, Scala3)
ThisBuild / scalaVersion := Scala3
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / githubWorkflowArtifactUpload := false

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@1.11", "adopt@1.16")
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep
    .Sbt(List("headerCheckAll", "scalafmtCheckAll", "scalafmtSbtCheck"), name = Some("Check headers+formatting")),
  WorkflowStep.Sbt(List("Test/compile"), name = Some("Compile")),
  WorkflowStep.Sbt(List("test"), name = Some("Run tests")),
  WorkflowStep.Sbt(List("doc", "docs/mdoc"), name = Some("Build docs"))
)

inThisBuild(
  List(
    organization := "org.typelevel",
    homepage := Some(url("https://typelevel.org/spire/")),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        "id_m",
        "Erik Osheim",
        "",
        url("https://github.com/non/")
      ),
      Developer(
        "tixxit",
        "Tom Switzer",
        "",
        url("https://github.com/tixxit/")
      )
    )
  )
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches +=
  RefPredicate.StartsWith(Ref.Tag("v"))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)
// Projects

lazy val spire = project
  .in(file("."))
  .settings(moduleName := "spire-root")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(spireJVM, spireJS)
  .dependsOn(spireJVM, spireJS)

lazy val spireJVM = project
  .in(file(".spireJVM"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(macros.jvm,
             core.jvm,
             data.jvm,
             extras.jvm,
             examples,
             laws.jvm,
             legacy.jvm,
             platform.jvm,
             tests.jvm,
             util.jvm,
             benchmark
  )
  .dependsOn(macros.jvm,
             core.jvm,
             data.jvm,
             extras.jvm,
             examples,
             laws.jvm,
             legacy.jvm,
             platform.jvm,
             tests.jvm,
             util.jvm,
             benchmark
  )

lazy val spireJS = project
  .in(file(".spireJS"))
  .settings(moduleName := "spire-aggregate")
  .settings(spireSettings)
  .settings(unidocSettings)
  .settings(noPublishSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(macros.js, core.js, data.js, extras.js, laws.js, legacy.js, platform.js, tests.js, util.js)
  .dependsOn(macros.js, core.js, data.js, extras.js, laws.js, legacy.js, platform.js, tests.js, util.js)
  .enablePlugins(ScalaJSPlugin)

lazy val platform = crossProject(JSPlatform, JVMPlatform)
  .settings(moduleName := "spire-platform")
  .settings(spireSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(macros, util)

lazy val macros = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-macros")
  .settings(spireSettings: _*)
  .settings(scalaCheckSettings: _*)
  .settings(munitSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)

lazy val data = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-data")
  .settings(spireSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)

lazy val legacy = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-legacy")
  .settings(spireSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)

lazy val util = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-util")
  .settings(spireSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(macros)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire")
  .settings(spireSettings: _*)
  .settings(coreSettings: _*)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(macros, platform, util)

lazy val extras = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-extras")
  .settings(spireSettings: _*)
  .settings(extrasSettings: _*)
  .settings(crossVersionSharedSources: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(macros, platform, util, core, data)
lazy val docs = project
  .in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .dependsOn(macros.jvm, core.jvm, extras.jvm)
  .settings(moduleName := "spire-docs")
  .settings(commonSettings: _*)
  .settings(spireSettings: _*)
  .settings(docSettings: _*)
  .settings(noPublishSettings)
  .enablePlugins(MdocPlugin)
  .settings(
    mdocIn := (Compile / sourceDirectory).value / "mdoc",
    mdocVariables := Map(
      "VERSION" -> (if (isSnapshot.value)
                      previousStableVersion.value.getOrElse(version.value)
                    else
                      version.value),
      "COPYRIGHT_TO" ->  java.time.LocalDate.now().getYear().toString(),
      "AUTHORS" -> "Erik Osheim, Tom Switzer"
    ),
    // NOTE: disable link hygine to supress dead link warnings because mdoc does not go well with Jekyll
    mdocExtraArguments ++= Seq("--no-link-hygiene")
  )
  .settings(commonJvmSettings: _*)

lazy val examples = project
  .settings(moduleName := "spire-examples")
  .settings(spireSettings)
  .settings(crossVersionSharedSources: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.apfloat" % "apfloat" % apfloatVersion,
      "org.jscience" % "jscience" % jscienceVersion
    )
  )
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .dependsOn(core.jvm, extras.jvm)

lazy val laws = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "spire-laws")
  .settings(spireSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra-laws" % algebraVersion,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion
    )
  )
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(core, extras)

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(moduleName := "spire-tests")
  .settings(spireSettings: _*)
  .settings(munitSettings: _*)
  .settings(noPublishSettings: _*)
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(core, data, legacy, extras, laws)

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .settings(moduleName := "spire-benchmark")
  .settings(spireSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.apfloat" % "apfloat" % apfloatVersion,
      "org.jscience" % "jscience" % jscienceVersion,
      "org.apache.commons" % "commons-math3" % apacheCommonsMath3Version
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core.jvm, extras.jvm)

// General settings

addCommandAlias(
  "validateJVM",
  ";coreJVM/scalastyle;macrosJVM/test;coreJVM/test;extrasJVM/test;lawsJVM/test;testsJVM/test;examples/test;benchmark/test"
)

addCommandAlias("validateJS", ";macrosJS/test;coreJS/test;extrasJS/test;lawsJS/test;testsJS/test")

addCommandAlias("validate", ";validateJVM;validateJS")

lazy val buildSettings = Seq()

lazy val commonDeps = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "algebra" % algebraVersion
  )
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.value.diff(
    Seq(
      "-Xfatal-warnings",
      "-language:existentials",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xcheck-macros"
    )
  ),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  headerLicense := Some(HeaderLicense.Custom(header))
) ++ scalaMacroDependencies ++ warnUnusedImport

lazy val commonJsSettings = Seq(
  Global / scalaJSStage := FastOptStage,
  Test / parallelExecution := false,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
)

lazy val commonJvmSettings = Seq()

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "Spire",
  micrositeDescription := "Powerful new number types and numeric abstractions for Scala",
  micrositeAuthor := "Spire contributors",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "https://typelevel.org/spire",
  micrositeBaseUrl := "spire",
  micrositeDocumentationUrl := "https://www.javadoc.io/doc/org.typelevel/spire_2.13/latest/spire/index.html",
  micrositeDocumentationLabelDescription := "API Documentation",
  micrositeExtraMdFiles := Map(
    file("CHANGES.md") -> ExtraMdFileConfig(
      "changes.md",
      "home",
      Map("title" -> "Changes", "section" -> "Home", "position" -> "2")
    ),
    file("CONTRIBUTING.md") -> ExtraMdFileConfig(
      "contributing.md",
      "home",
      Map("title" -> "Contributing", "section" -> "Home", "position" -> "3")
    ),
    file("DESIGN.md") -> ExtraMdFileConfig(
      "design.md",
      "home",
      Map("title" -> "Design notes", "section" -> "Home", "position" -> "4")
    ),
    file("FRIENDS.md") -> ExtraMdFileConfig(
      "friends.md",
      "home",
      Map("title" -> "Friends of Spire", "section" -> "Home", "position" -> "6")
    )
  ),
  micrositeGithubOwner := "typelevel",
  micrositeGithubRepo := "spire",
  micrositeTheme := "pattern",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"
  ),
  micrositeConfigYaml := ConfigYml(
    yamlCustomProperties = Map(
      "spireVersion" -> version.value,
      "scalaVersion" -> scalaVersion.value
    )
  ),
  autoAPIMappings := true,
  ScalaUnidoc / unidoc / unidocProjectFilter :=
    inProjects(platform.jvm, macros.jvm, data.jvm, legacy.jvm, util.jvm, core.jvm, extras.jvm, laws.jvm),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork := true,
  javaOptions += "-Xmx4G", // to have enough memory in forks
//  fork in (ScalaUnidoc, unidoc) := true,
  ScalaUnidoc / unidoc / scalacOptions ++= Seq(
    "-groups",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/main€{FILE_PATH}.scala",
    "-sourcepath",
    (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-diagrams"
  ),
  git.remoteRepo := "git@github.com:typelevel/spire.git",
  makeSite / includeFilter := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  Jekyll / includeFilter := (makeSite / includeFilter).value
)

lazy val scoverageSettings = Seq(
  coverageMinimumStmtTotal := 40,
  coverageFailOnMinimum := false,
  coverageHighlighting := true,
  coverageExcludedPackages := "spire\\.benchmark\\..*;spire\\.macros\\..*"
)

lazy val coreSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
  buildInfoPackage := "spire",
  Compile / sourceGenerators += (Compile / genProductTypes).taskValue,
  genProductTypes := {
    val scalaSource = (Compile / sourceManaged).value
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

lazy val scalaCheckSettings = Seq(libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test)

lazy val munitSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % munit % Test,
    "org.typelevel" %%% "discipline-munit" % munitDiscipline % Test
  )
)

lazy val spireSettings = buildSettings ++ commonSettings ++ commonDeps ++ scoverageSettings

lazy val unidocSettings = Seq(
  ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(examples, benchmark, tests.jvm)
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
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.map { dir: File =>
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((major, minor)) =>
            new File(s"${dir.getPath}_$major.$minor")
          case None =>
            sys.error("couldn't parse scalaBinaryVersion ${scalaBinaryVersion.value}")
        }
      }
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("3")) Seq.empty
    else Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided")
  }
)

lazy val commonScalacOptions = Def.setting(
  Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xcheck-macros"
  )
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if (n >= 11) && (n <= 12) =>
        Seq("-Ywarn-unused-import")
      case _ => Seq()
    }
  },
  Compile / console / scalacOptions ~= { _.filterNot("-Ywarn-unused-import" == _) },
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)
