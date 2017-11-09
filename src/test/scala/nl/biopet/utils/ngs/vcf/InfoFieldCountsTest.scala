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
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("EMPTY"), FieldMethod.All)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 4L
    stats.countsMap shouldBe Map()
  }

  @Test
  def testMax(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Max)
    intercept[Exception] {
      reader.foreach(stats.addRecord)
    }.getMessage shouldBe "Not supported method"
    reader.close()
  }

  @Test
  def testMin(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Min)
    intercept[Exception] {
      reader.foreach(stats.addRecord)
    }.getMessage shouldBe "Not supported method"
    reader.close()
  }

  @Test
  def testAvg(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Avg)
    intercept[Exception] {
      reader.foreach(stats.addRecord)
    }.getMessage shouldBe "Not supported method"
    reader.close()
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
  }

}
