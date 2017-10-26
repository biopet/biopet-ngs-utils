package nl.biopet.utils.ngs.vcf

import java.io.File
import java.nio.file.Files

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._

class SampleDistributionsTest extends BiopetTest {
  @Test
  def testAddRecords(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val distributions = new SampleDistributions
    reader.foreach(distributions.addRecord)
    reader.close()

    val map = distributions.toMap

    map(GenotypeStats.Total) shouldBe Map(3 -> 4L)
    map(GenotypeStats.Variant) shouldBe Map(2 -> 1L, 1 -> 1L, 3 -> 1L, 0 -> 1L)

    val tempDir = Files.createTempDirectory("sampleDistributions").toFile

    distributions.writeToDir(tempDir)
    for (x <- GenotypeStats.values) {
      val histogramFile = new File(tempDir, s"$x.tsv")
      val aggregateFile = new File(tempDir, s"$x.aggregate.tsv")
      histogramFile should exist
      aggregateFile should exist
    }
  }

  @Test
  def testCombine(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val distributions = new SampleDistributions
    reader.foreach(distributions.addRecord)
    reader.close()

    distributions += distributions

    val map = distributions.toMap

    map(GenotypeStats.Total) shouldBe Map(3 -> 8L)
    map(GenotypeStats.Variant) shouldBe Map(2 -> 2L, 1 -> 2L, 3 -> 2L, 0 -> 2L)

    val tempDir = Files.createTempDirectory("sampleDistributions").toFile

    distributions.writeToDir(tempDir)
    for (x <- GenotypeStats.values) {
      val histogramFile = new File(tempDir, s"$x.tsv")
      val aggregateFile = new File(tempDir, s"$x.aggregate.tsv")
      histogramFile should exist
      aggregateFile should exist
    }
  }
}
