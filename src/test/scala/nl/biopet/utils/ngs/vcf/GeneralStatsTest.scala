package nl.biopet.utils.ngs.vcf

import java.io.File
import java.nio.file.Files

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._
import scala.io.Source

class GeneralStatsTest extends BiopetTest {
  @Test
  def testGeneral(): Unit = {
    val stats = new GeneralStats
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    reader.foreach(stats.addRecord)
    reader.close()

    val map = stats.toMap
    map(GeneralStats.Total) shouldBe 4L
    map(GeneralStats.Indel) shouldBe 0L
    map(GeneralStats.SNP) shouldBe 4L
    map(GeneralStats.MonomorphicInSamples) shouldBe 1L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    stats.writeHistogramToTsv(new File(tempDir, "general.tsv"))
    val lines = Source.fromFile(new File(tempDir, "general.tsv")).getLines().toList
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(2)
    lines.head shouldBe "value\tcount"
    GeneralStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"${s.toString} not found")
    )
  }

  @Test
  def testCombine(): Unit = {
    val stats = new GeneralStats
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    reader.foreach(stats.addRecord)
    reader.close()

    stats += stats

    val map = stats.toMap
    map(GeneralStats.Total) shouldBe 8L
    map(GeneralStats.Indel) shouldBe 0L
    map(GeneralStats.SNP) shouldBe 8L
    map(GeneralStats.MonomorphicInSamples) shouldBe 2L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    stats.writeHistogramToTsv(new File(tempDir, "general.tsv"))
    val lines = Source.fromFile(new File(tempDir, "general.tsv")).getLines().toList
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(2)
    lines.head shouldBe "value\tcount"
    GeneralStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"$s not found")
    )
  }

}
