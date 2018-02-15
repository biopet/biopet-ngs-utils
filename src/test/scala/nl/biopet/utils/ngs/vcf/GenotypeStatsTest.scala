package nl.biopet.utils.ngs.vcf

import java.io.File
import java.nio.file.Files

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.io.Source
import scala.collection.JavaConversions._

class GenotypeStatsTest extends BiopetTest {
  @Test
  def testGeneral(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val stats = new GenotypeStats(reader.getFileHeader)

    reader.foreach(stats.addRecord)
    reader.close()

    val map = stats.toMap
    map("Sample_1")(GenotypeStats.Total) shouldBe 4L
    map("Sample_1")(GenotypeStats.Filtered) shouldBe 0L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    stats.writeToTsv(new File(tempDir, "genotype.tsv"))
    val lines =
      Source.fromFile(new File(tempDir, "genotype.tsv")).getLines().toList
    lines.head shouldBe stats.samples.keys.toList.sorted
      .mkString("Sample\t", "\t", "")
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(4)
    GenotypeStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"$s not found"))
  }

  @Test
  def testCombine(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val stats = new GenotypeStats(reader.getFileHeader)

    reader.foreach(stats.addRecord)
    reader.close()

    stats += stats

    val map = stats.toMap
    map("Sample_1")(GenotypeStats.Total) shouldBe 8L
    map("Sample_1")(GenotypeStats.Filtered) shouldBe 0L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    stats.writeToTsv(new File(tempDir, "genotype.tsv"))
    val lines =
      Source.fromFile(new File(tempDir, "genotype.tsv")).getLines().toList
    lines.head shouldBe stats.samples.keys.toList.sorted
      .mkString("Sample\t", "\t", "")
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(4)
    GenotypeStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"$s not found"))
  }

}
