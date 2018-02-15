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
import org.testng.annotations.Test

import scala.collection.JavaConversions._
import scala.io.Source

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
    val stats =
      new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Max)
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
    val stats =
      new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Min)
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
    val stats =
      new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Avg)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 1,
                                 "1.3333333333333333" -> 1,
                                 "3.0" -> 1,
                                 "2.0" -> 1)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("1.0" -> 2,
                                 "1.3333333333333333" -> 2,
                                 "3.0" -> 2,
                                 "2.0" -> 2)
  }

  @Test
  def testAll(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats =
      new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.All)
    reader.foreach(stats.addRecord)
    reader.close()

    stats.total shouldBe 4L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("2" -> 2L, "1" -> 3L, "3" -> 1L)

    stats += stats
    stats.total shouldBe 8L
    stats.noValue shouldBe 0L
    stats.countsMap shouldBe Map("2" -> 4L, "1" -> 6L, "3" -> 2L)

    val outputFilr = File.createTempFile("test.", ".tsv")
    stats.writeHistogram(outputFilr)
    val lines = Source.fromFile(outputFilr).getLines().toList
    lines.head shouldBe "value\tcount"
    lines(1) shouldBe "1\t6"
    lines(2) shouldBe "2\t4"
    lines(3) shouldBe "3\t2"

    val histogram = stats.asHistrogram
    histogram.countsMap shouldBe Map(2.0 -> 4L, 1.0 -> 6L, 3.0 -> 2L)
  }

  @Test
  def testUnique(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)
    val header = reader.getFileHeader
    val stats =
      new InfoFieldCounts(header.getInfoHeaderLine("DP"), FieldMethod.Unique)
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
