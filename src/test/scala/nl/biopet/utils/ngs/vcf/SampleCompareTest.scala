package nl.biopet.utils.ngs.vcf

import java.io.File
import java.nio.file.Files

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._
import scala.io.Source

class SampleCompareTest extends BiopetTest {
  @Test
  def testAddRecords(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, None))
    reader.close()

    compare.allelesCount("Sample_1")("Sample_2") shouldBe 7L
    compare.allelesCount(compare.samples("Sample_1"))(compare.samples("Sample_2")) shouldBe 7L
    compare.genotypesCount("Sample_1")("Sample_2") shouldBe 3L
    compare.genotypesCount(compare.samples("Sample_1"))(compare.samples("Sample_2")) shouldBe 3L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    compare.writeAllFiles(tempDir)
    new File(tempDir, "allele.abs.tsv") should exist
    new File(tempDir, "allele.rel.tsv") should exist
    new File(tempDir, "genotype.abs.tsv") should exist
    new File(tempDir, "genotype.rel.tsv") should exist

    Source.fromFile(new File(tempDir, "allele.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t8\t7\t6",
      "Sample_2\t7\t8\t7",
      "Sample_3\t6\t7\t8"
    )

    Source.fromFile(new File(tempDir, "allele.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.875\t0.75",
      "Sample_2\t0.875\t1.0\t0.875",
      "Sample_3\t0.75\t0.875\t1.0"
    )

    Source.fromFile(new File(tempDir, "genotype.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t4\t3\t2",
      "Sample_2\t3\t4\t3",
      "Sample_3\t2\t3\t4"
    )
    Source.fromFile(new File(tempDir, "genotype.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.75\t0.5",
      "Sample_2\t0.75\t1.0\t0.75",
      "Sample_3\t0.5\t0.75\t1.0"
    )
  }

  @Test
  def testCombine(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, None))
    reader.close()

    compare += compare

    compare.allelesCount("Sample_1")("Sample_2") shouldBe 14L
    compare.allelesCount(compare.samples("Sample_1"))(compare.samples("Sample_2")) shouldBe 14L
    compare.genotypesCount("Sample_1")("Sample_2") shouldBe 6L
    compare.genotypesCount(compare.samples("Sample_1"))(compare.samples("Sample_2")) shouldBe 6L

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    compare.writeAllFiles(tempDir)
    new File(tempDir, "allele.abs.tsv") should exist
    new File(tempDir, "allele.rel.tsv") should exist
    new File(tempDir, "genotype.abs.tsv") should exist
    new File(tempDir, "genotype.rel.tsv") should exist

    Source.fromFile(new File(tempDir, "allele.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t16\t14\t12",
      "Sample_2\t14\t16\t14",
      "Sample_3\t12\t14\t16"
    )

    Source.fromFile(new File(tempDir, "allele.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.875\t0.75",
      "Sample_2\t0.875\t1.0\t0.875",
      "Sample_3\t0.75\t0.875\t1.0"
    )

    Source.fromFile(new File(tempDir, "genotype.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t8\t6\t4",
      "Sample_2\t6\t8\t6",
      "Sample_3\t4\t6\t8"
    )
    Source.fromFile(new File(tempDir, "genotype.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.75\t0.5",
      "Sample_2\t0.75\t1.0\t0.75",
      "Sample_3\t0.5\t0.75\t1.0"
    )
  }

  @Test
  def testDepthFilter(): Unit = {
    val reader = new VCFFileReader(resourceFile("/multi.vcf"), false)

    val compare = new SampleCompare(reader.getFileHeader)
    reader.foreach(compare.addRecord(_, Some(5)))
    reader.close()

    val tempDir = Files.createTempDirectory("sampleCompare").toFile
    compare.writeAllFiles(tempDir)
    new File(tempDir, "allele.abs.tsv") should exist
    new File(tempDir, "allele.rel.tsv") should exist
    new File(tempDir, "genotype.abs.tsv") should exist
    new File(tempDir, "genotype.rel.tsv") should exist

    Source.fromFile(new File(tempDir, "allele.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t4\t1\t3",
      "Sample_2\t1\t4\t3",
      "Sample_3\t3\t3\t6"
    )

    Source.fromFile(new File(tempDir, "allele.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.25\t0.75",
      "Sample_2\t0.25\t1.0\t0.75",
      "Sample_3\t0.5\t0.5\t1.0"
    )

    Source.fromFile(new File(tempDir, "genotype.abs.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t2\t0\t1",
      "Sample_2\t0\t2\t1",
      "Sample_3\t1\t1\t3"
    )
    Source.fromFile(new File(tempDir, "genotype.rel.tsv")).getLines().toList shouldBe List(
      "Sample\tSample_1\tSample_2\tSample_3",
      "Sample_1\t1.0\t0.0\t0.5",
      "Sample_2\t0.0\t1.0\t0.5",
      "Sample_3\t0.3333333333333333\t0.3333333333333333\t1.0"
    )
  }

}
