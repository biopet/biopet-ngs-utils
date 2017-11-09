package nl.biopet.utils.ngs.vcf

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._

class InfoFieldHistogramTest extends BiopetTest {
  @Test
  def testNonExist(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("EMPTY"), FieldMethod.Max)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 4L
    stats.countsMap shouldBe Map()
    stats.aggregateStats shouldBe Map()
  }

  @Test
  def testMax(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("DP"), FieldMethod.Max)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map(1.0 -> 1L, 2.0 -> 2L, 3.0 -> 1L)
    stats.aggregateStats shouldBe Map("modal" -> 2.0, "mean" -> 2.0, "min" -> 1.0, "max" -> 3.0, "median" -> 1.0)
  }

  @Test
  def testMin(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("DP"), FieldMethod.Min)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map(1.0 -> 2L, 2.0 -> 1L, 3.0 -> 1L)
    stats.aggregateStats shouldBe Map("modal" -> 1.0, "mean" -> 1.75, "min" -> 1.0, "max" -> 3.0, "median" -> 2.0)
  }

  @Test
  def testAvg(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("DP"), FieldMethod.Avg)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map(1.0 -> 1L, 1.3333333333333333 -> 1L, 2.0 -> 1L, 3.0 -> 1L)
    stats.aggregateStats shouldBe Map("modal" -> 2.0, "mean" -> 1.8333333333333333, "min" -> 1.0, "max" -> 3.0, "median" -> 1.0)
  }

  @Test
  def testAll(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("DP"), FieldMethod.All)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map(1.0 -> 3L, 2.0 -> 2L, 3.0 -> 1L)
    stats.aggregateStats shouldBe Map("modal" -> 1.0, "mean" -> 1.6666666666666667, "min" -> 1.0, "max" -> 3.0, "median" -> 2.0)
  }

  @Test
  def testUnique(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new InfoFieldHistogram(header.getInfoHeaderLine("DP"), FieldMethod.Unique)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map(1.0 -> 2L, 2.0 -> 2L, 3.0 -> 1L)
    stats.aggregateStats shouldBe Map("modal" -> 2.0, "mean" -> 1.8, "min" -> 1.0, "max" -> 3.0, "median" -> 2.0)
  }

}
