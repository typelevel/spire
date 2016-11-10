scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addSbtPlugin("com.jsuereth"        % "sbt-pgp"               % "1.0.0")
addSbtPlugin("com.github.gseitz"   % "sbt-release"           % "1.0.0")
addSbtPlugin("com.eed3si9n"        % "sbt-unidoc"            % "0.3.3")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"         % "0.3.2")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"               % "0.1.6")
addSbtPlugin("org.scoverage"       % "sbt-scoverage"         % "1.5.0")
addSbtPlugin("org.scalastyle"     %% "scalastyle-sbt-plugin" % "0.7.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-git"               % "0.8.4")
addSbtPlugin("org.scala-js"        % "sbt-scalajs"           % "0.6.13")
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.12"
