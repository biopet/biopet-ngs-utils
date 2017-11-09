package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.{DataProvider, Test}

import scala.collection.JavaConversions._
import scala.io.Source

class GenotypeFieldHistogramTest extends BiopetTest {
  @Test
  def testNonExist(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new GenotypeFieldHistogram(header, header.getFormatHeaderLine("EMPTY"), FieldMethod.All)
    reader.foreach(stats.addRecord(_))
    reader.close()

    stats.total shouldBe List(4, 4, 4)
    stats.noValue shouldBe List(4, 4, 4)
    stats.countsMap shouldBe Map(2 -> Map(), 1 -> Map(), 0 -> Map())
  }

  @DataProvider(name = "correctMethod")
  def correctMethod: Array[Array[FieldMethod.Value]] = {
    Array(
      Array(FieldMethod.All),
      Array(FieldMethod.Unique),
      Array(FieldMethod.Max),
      Array(FieldMethod.Min),
      Array(FieldMethod.Avg)
    )
  }

  @Test(dataProvider = "correctMethod")
  def testCorrect(method: FieldMethod.Value): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new GenotypeFieldHistogram(header, header.getFormatHeaderLine("DP"), method)
    reader.foreach(stats.addRecord(_))
    reader.close()

    stats.total shouldBe List(4, 4, 4)
    stats.noValue shouldBe List(1, 1, 1)
    stats.countsMap shouldBe Map(
      2 -> Map(5.0 -> 3),
      1 -> Map(5.0 -> 2, 1.0 -> 1),
      0 -> Map(5.0 -> 2, 1.0 -> 1)
    )
    val output = File.createTempFile("test,", ".tsv")
    output.deleteOnExit()
    stats.writeToFile(output)

    val lines = Source.fromFile(output).getLines().toList
    lines.head shouldBe "Sample\tSample_3\tSample_2\tSample_1"
    lines.tail shouldBe List("1.0\t0\t1\t1", "5.0\t3\t2\t2")
  }
}
