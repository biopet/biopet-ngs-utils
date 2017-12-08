package nl.biopet.utils.ngs.vcf

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._

class InfoFieldCountsTest extends BiopetTest {
  @Test
  def testNonExist(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val vcfField = VcfField("EMPTY", FieldMethod.All)
    val stats = vcfField.newInfoCount(header)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 4L
    stats.countsMap shouldBe Map()

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 8L
    stats.countsMap shouldBe Map()
  }

  @Test
  def testMax(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Max)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 1, "3.0" -> 1, "2.0" -> 2)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 2, "3.0" -> 2, "2.0" -> 4)
  }

  @Test
  def testMin(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Min)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 2, "3.0" -> 1, "2.0" -> 1)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 4, "3.0" -> 2, "2.0" -> 2)
  }

  @Test
  def testAvg(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Avg)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 1, "1.3333333333333333" -> 1, "3.0" -> 1, "2.0" -> 1)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 2, "1.3333333333333333" -> 2, "3.0" -> 2, "2.0" -> 2)
  }

  @Test
  def testAll(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.All)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("2" -> 2L, "1" -> 3L, "3" -> 1L)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("2" -> 4L, "1" -> 6L, "3" -> 2L)
  }

  @Test
  def testUnique(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Unique)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1" -> 2L, "2" -> 2L, "3" -> 1L)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1" -> 4L, "2" -> 4L, "3" -> 2L)
  }

}
