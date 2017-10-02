organization := "com.github.biopet"
name := "biopet-ngs-utils"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"

resolvers += Resolver.mavenLocal

libraryDependencies += "com.github.samtools" % "htsjdk" % "2.11.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % Test
libraryDependencies += "org.testng" % "testng" % "6.8" % Test
libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % Test
