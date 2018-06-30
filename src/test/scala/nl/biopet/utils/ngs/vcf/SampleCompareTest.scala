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

class SampleCompareTest extends BiopetTest {

  def outputAlleleAbs(outputDir: File) = new File(outputDir, "allele.abs.tsv")
  def outputAlleleRel(outputDir: File) = new File(outputDir, "allele.rel.tsv")
  def outputGenotypeAbs(outputDir: File) =
    new File(outputDir, "genotype.abs.tsv")
  def outputGenotypeRel(outputDir: File) =
    new File(outputDir, "genotype.rel.tsv")
  def outputGenotypeRef(outputDir: File) =
    new File(outputDir, "genotype.ref.abs.tsv")
  def outputAlleleRef(outputDir: File) =
    new File(outputDir, "allele.ref.abs.tsv")
  def outputGenotypeNonref(outputDir: File) =
    new File(outputDir, "genotype.non_ref.abs.tsv")
  def outputAlleleNonref(outputDir: File) =
    new File(outputDir, "allele.non_ref.abs.tsv")

  val addRecordsDir: File = Files.createTempDirectory("sampleCompare").toFile

  @Test(groups = Array("generate"))
  def testAddRecords(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, None))
    reader.close()

    compare.allelesCount("Sample_1")("Sample_2") shouldBe 7L
    compare.allelesCount(compare.samples("Sample_1"))(
      compare.samples("Sample_2")) shouldBe 7L
    compare.genotypesCount("Sample_1")("Sample_2") shouldBe 3L
    compare.genotypesCount(compare.samples("Sample_1"))(
      compare.samples("Sample_2")) shouldBe 3L

    compare.writeAllFiles(addRecordsDir)
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsAlleleAbs(): Unit = {
    val file = outputAlleleAbs(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t8\t7\t6",
      "Sample_2\t7\t8\t7",
      "Sample_3\t6\t7\t8"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsAlleleRel(): Unit = {
    val file = outputAlleleRel(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.875\t0.75",
      "Sample_2\t0.875\t1.0\t0.875",
      "Sample_3\t0.75\t0.875\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsGenotypeAbs(): Unit = {
    val file = outputGenotypeAbs(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t4\t3\t2",
      "Sample_2\t3\t4\t3",
      "Sample_3\t2\t3\t4"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsGenotypeRel(): Unit = {
    val file = outputGenotypeRel(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.75\t0.5",
      "Sample_2\t0.75\t1.0\t0.75",
      "Sample_3\t0.5\t0.75\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsGenotypeRef(): Unit = {
    val file = outputGenotypeRef(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1\t1\t1",
      "Sample_2\t1\t2\t2",
      "Sample_3\t1\t2\t3"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsAlleleRef(): Unit = {
    val file = outputAlleleRef(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t5\t5\t5",
      "Sample_2\t5\t6\t6",
      "Sample_3\t5\t6\t7"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsGenotypeNonref(): Unit = {
    val file = outputGenotypeNonref(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t3\t2\t1",
      "Sample_2\t2\t2\t1",
      "Sample_3\t1\t1\t1"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testAddRecordsAlleleNonref(): Unit = {
    val file = outputAlleleNonref(addRecordsDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t3\t2\t1",
      "Sample_2\t2\t2\t1",
      "Sample_3\t1\t1\t1"
    )
  }

  val combineDir: File = Files.createTempDirectory("sampleCompare").toFile

  @Test(groups = Array("generate"))
  def testCombine(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, None))
    reader.close()

    compare += compare

    compare.allelesCount("Sample_1")("Sample_2") shouldBe 14L
    compare.allelesCount(compare.samples("Sample_1"))(
      compare.samples("Sample_2")) shouldBe 14L
    compare.genotypesCount("Sample_1")("Sample_2") shouldBe 6L
    compare.genotypesCount(compare.samples("Sample_1"))(
      compare.samples("Sample_2")) shouldBe 6L

    compare.writeAllFiles(combineDir)
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineAlleleAbs(): Unit = {
    val file = outputAlleleAbs(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t16\t14\t12",
      "Sample_2\t14\t16\t14",
      "Sample_3\t12\t14\t16"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineAlleleRel(): Unit = {
    val file = outputAlleleRel(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.875\t0.75",
      "Sample_2\t0.875\t1.0\t0.875",
      "Sample_3\t0.75\t0.875\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineGenotypeAbs(): Unit = {
    val file = outputGenotypeAbs(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t8\t6\t4",
      "Sample_2\t6\t8\t6",
      "Sample_3\t4\t6\t8"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineGenotypeRel(): Unit = {
    val file = outputGenotypeRel(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.75\t0.5",
      "Sample_2\t0.75\t1.0\t0.75",
      "Sample_3\t0.5\t0.75\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineAlleleRef(): Unit = {
    val file = outputAlleleRef(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t10\t10\t10",
      "Sample_2\t10\t12\t12",
      "Sample_3\t10\t12\t14"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineGenotypeNonref(): Unit = {
    val file = outputGenotypeNonref(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t6\t4\t2",
      "Sample_2\t4\t4\t2",
      "Sample_3\t2\t2\t2"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testCombineAlleleNonref(): Unit = {
    val file = outputAlleleNonref(combineDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t6\t4\t2",
      "Sample_2\t4\t4\t2",
      "Sample_3\t2\t2\t2"
    )
  }

  val depthFilterDir: File = Files.createTempDirectory("sampleCompare").toFile

  @Test(groups = Array("generate"))
  def testDepthFilter(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, Some(5)))
    reader.close()

    compare.writeAllFiles(depthFilterDir)
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterAlleleRel(): Unit = {
    val file = outputAlleleRel(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.25\t0.75",
      "Sample_2\t0.25\t1.0\t0.75",
      "Sample_3\t0.5\t0.5\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterGenptypeAbs(): Unit = {
    val file = outputGenotypeAbs(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t2\t0\t1",
      "Sample_2\t0\t2\t1",
      "Sample_3\t1\t1\t3"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterGenotypeRel(): Unit = {
    val file = outputGenotypeRel(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.0\t0.5",
      "Sample_2\t0.0\t1.0\t0.5",
      "Sample_3\t0.3333333333333333\t0.3333333333333333\t1.0"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterGenotypeRef(): Unit = {
    val file = outputGenotypeRef(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t0\t0\t0",
      "Sample_2\t0\t1\t1",
      "Sample_3\t0\t1\t2"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterAlleleRef(): Unit = {
    val file = outputAlleleRef(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t2\t1\t2",
      "Sample_2\t1\t3\t3",
      "Sample_3\t2\t3\t5"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterGenotypeNonref(): Unit = {
    val file = outputGenotypeNonref(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t2\t0\t1",
      "Sample_2\t0\t1\t0",
      "Sample_3\t1\t0\t1"
    )
  }

  @Test(dependsOnGroups = Array("generate"))
  def testDepthFilterAlleleNonref(): Unit = {
    val file = outputAlleleNonref(depthFilterDir)
    file should exist
    Source
      .fromFile(file)
      .getLines()
      .toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t2\t0\t1",
      "Sample_2\t0\t1\t0",
      "Sample_3\t1\t0\t1"
    )
  }
}
