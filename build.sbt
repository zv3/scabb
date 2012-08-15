name := "scabb"

version := "0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12" % "test"
)

