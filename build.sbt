organization := "com.github.biopet"
organizationName := "Biopet"
name := "ngs-utils"

biopetUrlName := "ngs-utils"

startYear := Some(2014)

biopetIsTool := false

developers += Developer(id = "ffinfo",
                        name = "Peter van 't Hof",
                        email = "pjrvanthof@gmail.com",
                        url = url("https://github.com/ffinfo"))

crossScalaVersions := Seq("2.11.12", "2.12.5")

scalaVersion := "2.11.12"

libraryDependencies += "com.github.biopet" %% "common-utils" % "0.7"
libraryDependencies += "com.github.samtools" % "htsjdk" % "2.15.1"

libraryDependencies += "com.github.biopet" %% "test-utils" % "0.4" % Test
