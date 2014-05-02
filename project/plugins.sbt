logLevel := Level.Warn

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("com.sksamuel.scoverage" % "sbt-scoverage" % "0.98.2")

addSbtPlugin("com.sksamuel.scoverage" %% "sbt-coveralls" % "0.0.5")

//addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.1.4")
