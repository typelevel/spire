scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

val sbtTypelevelVersion = "0.4.5"
addSbtPlugin("org.typelevel" % "sbt-typelevel" % sbtTypelevelVersion)
addSbtPlugin("org.typelevel" % "sbt-typelevel-site" % sbtTypelevelVersion)

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.3")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.35"
