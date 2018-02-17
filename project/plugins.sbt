scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addSbtPlugin("com.jsuereth"        % "sbt-pgp"               % "1.1.0")
addSbtPlugin("com.github.gseitz"   % "sbt-release"           % "1.0.7")
addSbtPlugin("com.eed3si9n"        % "sbt-unidoc"            % "0.4.1")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"         % "0.7.0")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"               % "0.3.3")
addSbtPlugin("org.scoverage"       % "sbt-scoverage"         % "1.5.1")
addSbtPlugin("org.scalastyle"     %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-git"               % "0.9.3")
addSbtPlugin("org.xerial.sbt"      % "sbt-sonatype"          % "2.3")
addSbtPlugin("org.scala-js"        % "sbt-scalajs"           % "0.6.22")
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.12"
