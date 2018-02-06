organization := "com.github.biopet"
name := "NgsUtils"

biopetUrlName := "ngs-utils"

biopetIsTool := false

developers += Developer(id="ffinfo", name="Peter van 't Hof", email="pjrvanthof@gmail.com", url=url("https://github.com/ffinfo"))

scalaVersion := "2.11.11"

libraryDependencies += "com.github.biopet" %% "CommonUtils" % "0.3-SNAPSHOT" changing()
libraryDependencies += "com.github.samtools" % "htsjdk" % "2.14.1"

libraryDependencies += "com.github.biopet" %% "TestUtils" % "0.3-SNAPSHOT" % Test changing()
