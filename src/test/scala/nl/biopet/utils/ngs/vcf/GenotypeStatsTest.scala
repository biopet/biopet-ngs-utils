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
    lines.headOption shouldBe Some(stats.samples.keys.toList.sorted)
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
    lines.headOption shouldBe Some(stats.samples.keys.toList.sorted)
      .mkString("Sample\t", "\t", "")
    lines.map(_.split("\t")).map(_.length).distinct shouldBe List(4)
    GenotypeStats.values.foreach(s =>
      require(lines.exists(_.startsWith(s.toString + "\t")), s"$s not found"))
  }

}
