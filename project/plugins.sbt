scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers += Resolver.sonatypeRepo("snapshots")
addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.4-91c982a-SNAPSHOT")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.2")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.24")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.3")

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.32"
