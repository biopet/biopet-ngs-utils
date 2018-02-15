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

class SampleDistributionsTest extends BiopetTest {
  @Test
  def testAddRecords(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val distributions = new SampleDistributions
    reader.foreach(distributions.addRecord)
    reader.close()

    val map = distributions.toMap

    map(GenotypeStats.Total) shouldBe Map(3 -> 4L)
    map(GenotypeStats.Variant) shouldBe Map(2 -> 1L, 1 -> 1L, 3 -> 1L, 0 -> 1L)

    val tempDir = Files.createTempDirectory("sampleDistributions").toFile

    distributions.writeToDir(tempDir)
    for (x <- GenotypeStats.values) {
      val histogramFile = new File(tempDir, s"$x.tsv")
      val aggregateFile = new File(tempDir, s"$x.aggregate.tsv")
      histogramFile should exist
      aggregateFile should exist
    }
  }

  @Test
  def testCombine(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val distributions = new SampleDistributions
    reader.foreach(distributions.addRecord)
    reader.close()

    distributions += distributions

    val map = distributions.toMap

    map(GenotypeStats.Total) shouldBe Map(3 -> 8L)
    map(GenotypeStats.Variant) shouldBe Map(2 -> 2L, 1 -> 2L, 3 -> 2L, 0 -> 2L)

    val tempDir = Files.createTempDirectory("sampleDistributions").toFile

    distributions.writeToDir(tempDir)
    for (x <- GenotypeStats.values) {
      val histogramFile = new File(tempDir, s"$x.tsv")
      val aggregateFile = new File(tempDir, s"$x.aggregate.tsv")
      histogramFile should exist
      aggregateFile should exist
    }
  }
}
