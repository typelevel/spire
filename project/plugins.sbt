scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.1")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.23")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.3")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.13.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.32"
