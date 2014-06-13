name := "spire-mbox-exp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scala-miniboxing.plugins" %% 
                       "miniboxing-runtime" % "0.3-SNAPSHOT"

addCompilerPlugin("org.scala-miniboxing.plugins" %% 
                  "miniboxing-plugin" % "0.3-SNAPSHOT")

scalacOptions += "-optimize"
