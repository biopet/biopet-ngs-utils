organization := "com.github.biopet"
name := "ngs-utils"

scalaVersion := "2.11.11"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.github.biopet" %% "common-utils" % "0.1-SNAPSHOT" changing()
libraryDependencies += "com.github.samtools" % "htsjdk" % "2.11.0"

libraryDependencies += "com.github.biopet" %% "test-utils" % "0.1-SNAPSHOT" % Test changing()
libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % Test

useGpg := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

import ReleaseTransformations._
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
