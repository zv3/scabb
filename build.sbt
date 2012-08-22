name := "scabb"

version := "0.1-SNAPSHOT"

licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12" % "test"
)

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/roman-kashitsyn/scabb</url>
  <scm>
    <url>git@github.com:roman-kashitsyn/scabb.git</url>
    <connection>scm:git:git@github.com:roman-kashitsyn/scabb.git</connection>
  </scm>
  <developers>
    <developer>
      <id>roman-kashitsyn</id>
      <name>Roman Kashitsyn</name>
      <url>http://your.url</url>
    </developer>
  </developers>
)
