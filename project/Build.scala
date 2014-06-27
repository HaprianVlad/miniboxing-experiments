import sbt._
import Keys._
import Process._
import sbt.Keys._

object MiniboxingBuild extends Build {

  // http://stackoverflow.com/questions/6506377/how-to-get-list-of-dependency-jars-from-an-sbt-0-10-0-project
  val getJars = TaskKey[Unit]("get-jars")
  val getJarsTask = getJars <<= (target, fullClasspath in Runtime) map { (target, cp) =>
    println("Target path is: "+target)
    println("Full classpath is: "+cp.map(_.data).mkString(":"))
  }

  val defaults = Defaults.defaultSettings ++ Seq(
    scalaSource in Compile := baseDirectory.value / "src",
    javaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    javaSource in Test := baseDirectory.value / "test",

    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

   
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),

    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint"),
    scalacOptions ++= Seq("-optimize", "-Yinline-warnings"),

    scalaVersion := "2.11.1",

    libraryDependencies ++=
      Seq("org.spire-math" %% "spire" % "0.7.4", 
				 // comparisons
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1",
      "org.apache.commons" % "commons-math3" % "3.2",

      // thyme
      "ichi.bench" % "thyme" % "0.1.0" from "http://plastic-idolatry.com/jars/thyme-0.1.0.jar",

      // caliper stuff
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1")
  )

  val scalaMeter = {
    val sMeter = Seq("com.github.axel22" %% "scalameter" % "0.5-M2")
    Seq(
      libraryDependencies ++= sMeter,
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    )
  }

  val junitDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.11.4",
      "com.novocode" % "junit-interface" % "0.10-M2"
    ),
    parallelExecution in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
  )

/** Settings for the miniboxing plugin */
  lazy val miniboxingSettings = Seq[Setting[_]](
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies += "org.scala-miniboxing.plugins" %% "miniboxing-runtime" % "0.3-SNAPSHOT",
    addCompilerPlugin("org.scala-miniboxing.plugins" %% "miniboxing-plugin" % "0.3-SNAPSHOT"),
    scalacOptions ++= (
      //"-P:minibox:log" ::    // enable the miniboxing plugin output
      //                       // (which explains what the plugin is doing)
      //"-P:minibox:hijack" :: // enable hijacking the @specialized annotations
      //                       // transforming them into @miniboxed annotations
      "-optimize" ::         // necessary to get the best performance when
                             // using the miniboxing plugin
      Nil
    )
  )


  lazy val _mboxing = Project(id = "spire-mbox", base = file("."), settings = defaults) aggregate (example)
  lazy val example = Project(id = "spire-mbox-example", base = file("components/example"), settings = defaults ++ scalaMeter ++ junitDeps ++ miniboxingSettings)
}
