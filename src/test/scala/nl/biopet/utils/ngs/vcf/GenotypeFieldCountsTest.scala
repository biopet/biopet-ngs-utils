/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.{DataProvider, Test}

import scala.collection.JavaConversions._
import scala.io.Source

class GenotypeFieldCountsTest extends BiopetTest {
  @Test
  def testNonExist(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats = new GenotypeFieldCounts(header,
                                        header.getFormatHeaderLine("EMPTY"),
                                        FieldMethod.All)
    reader.foreach(stats.addRecord(_))
    reader.close()

    stats.total shouldBe Map("Sample_3" -> 4, "Sample_2" -> 4, "Sample_1" -> 4)
    stats.noValue shouldBe Map("Sample_3" -> 4,
                               "Sample_2" -> 4,
                               "Sample_1" -> 4)
    stats.countsMap shouldBe Map("Sample_3" -> Map(),
                                 "Sample_2" -> Map(),
                                 "Sample_1" -> Map())

    stats += stats
    stats.total shouldBe Map("Sample_3" -> 8, "Sample_2" -> 8, "Sample_1" -> 8)
    stats.noValue shouldBe Map("Sample_3" -> 8,
                               "Sample_2" -> 8,
                               "Sample_1" -> 8)
    stats.countsMap shouldBe Map("Sample_3" -> Map(),
                                 "Sample_2" -> Map(),
                                 "Sample_1" -> Map())
  }

  @DataProvider(name = "inCorrectMethod")
  def inCorrectMethod: Array[Array[FieldMethod.Value]] = {
    Array(Array(FieldMethod.Max),
          Array(FieldMethod.Min),
          Array(FieldMethod.Avg))
  }

  @Test(dataProvider = "inCorrectMethod")
  def testIncorrect(method: FieldMethod.Value): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val vcfField = VcfField("DP", method)
    val stats = vcfField.newGenotypeCount(header)
    reader.foreach(stats.addRecord(_))
    reader.close()

    stats.total shouldBe Map("Sample_3" -> 4, "Sample_2" -> 4, "Sample_1" -> 4)
    stats.noValue shouldBe Map("Sample_3" -> 1,
                               "Sample_2" -> 1,
                               "Sample_1" -> 1)
    stats.countsMap shouldBe Map(
      "Sample_3" -> Map("5.0" -> 3),
      "Sample_2" -> Map("5.0" -> 2, "1.0" -> 1),
      "Sample_1" -> Map("5.0" -> 2, "1.0" -> 1)
    )
    val output = File.createTempFile("test,", ".tsv")
    output.deleteOnExit()
    stats.writeToFile(output)

    val lines = Source.fromFile(output).getLines().toList
    lines.head shouldBe "Sample\tSample_3\tSample_2\tSample_1"
    lines.tail shouldBe List("1.0\t0\t1\t1", "5.0\t3\t2\t2")
  }

  @DataProvider(name = "correctMethod")
  def correctMethod: Array[Array[FieldMethod.Value]] = {
    Array(Array(FieldMethod.All), Array(FieldMethod.Unique))
  }

  @Test(dataProvider = "correctMethod")
  def testCorrect(method: FieldMethod.Value): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats =
      new GenotypeFieldCounts(header, header.getFormatHeaderLine("DP"), method)
    reader.foreach(stats.addRecord(_))
    reader.close()

    stats.total shouldBe Map("Sample_3" -> 4, "Sample_2" -> 4, "Sample_1" -> 4)
    stats.noValue shouldBe Map("Sample_3" -> 1,
                               "Sample_2" -> 1,
                               "Sample_1" -> 1)
    stats.countsMap shouldBe Map(
      "Sample_3" -> Map("5" -> 3),
      "Sample_2" -> Map("5" -> 2, "1" -> 1),
      "Sample_1" -> Map("5" -> 2, "1" -> 1)
    )
    val output = File.createTempFile("test,", ".tsv")
    output.deleteOnExit()
    stats.writeToFile(output)

    val lines = Source.fromFile(output).getLines().toList
    lines.head shouldBe "Sample\tSample_3\tSample_2\tSample_1"
    lines.tail shouldBe List("1\t0\t1\t1", "5\t3\t2\t2")
  }
}
