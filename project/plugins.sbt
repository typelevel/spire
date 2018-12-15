scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addSbtPlugin("com.jsuereth"        % "sbt-pgp"               % "1.1.1")
addSbtPlugin("com.github.gseitz"   % "sbt-release"           % "1.0.8")
addSbtPlugin("com.eed3si9n"        % "sbt-unidoc"            % "0.4.2")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"         % "0.9.0")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"               % "0.3.4")
addSbtPlugin("org.scoverage"       % "sbt-scoverage"         % "1.5.1")
addSbtPlugin("org.scalastyle"     %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-git"               % "1.0.0")
addSbtPlugin("org.xerial.sbt"      % "sbt-sonatype"          % "2.3")
addSbtPlugin("org.scala-js"        % "sbt-scalajs"           % "0.6.23")
addSbtPlugin("org.tpolecat"        % "tut-plugin"            % "0.6.3")
addSbtPlugin("net.virtual-void"    % "sbt-dependency-graph"  % "0.9.0")
addSbtPlugin("org.portable-scala"  % "sbt-scalajs-crossproject" % "0.5.0")
addSbtPlugin("com.47deg"           % "sbt-microsites"        % "0.7.21")
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.12"


addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.8")
addSbtPlugin("com.eed3si9n"      % "sbt-unidoc"  % "0.4.1")
