organization := "org.bbcode"

name := "scabb"

version := "0.1-SNAPSHOT"

licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.11.7"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "org.specs2"             %% "specs2-core" % "3.6.4" % "test",
  "org.specs2"             %% "specs2-matcher-extra" % "3.6.4" % "test"   // For XmlMatchers
)