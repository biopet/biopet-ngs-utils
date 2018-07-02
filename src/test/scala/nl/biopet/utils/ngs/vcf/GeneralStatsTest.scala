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
    stats.writeToTsv(new File(tempDir, "general.tsv"))
    val lines =
      Source.fromFile(new File(tempDir, "general.tsv")).getLines().toList
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(2)
    lines.headOption shouldBe Some("value\tcount")
    GeneralStats.values.foreach(
      s =>
        require(lines.exists(_.startsWith(s.toString + "\t")),
                s"${s.toString} not found"))
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
    stats.writeToTsv(new File(tempDir, "general.tsv"))
    val lines =
      Source.fromFile(new File(tempDir, "general.tsv")).getLines().toList
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(2)
    lines.headOption shouldBe Some("value\tcount")
    GeneralStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"$s not found"))
  }

}
