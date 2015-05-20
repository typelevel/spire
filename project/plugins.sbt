scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.1.6")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.6.0")