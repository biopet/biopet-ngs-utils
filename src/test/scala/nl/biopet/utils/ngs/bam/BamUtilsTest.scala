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

package nl.biopet.utils.ngs.bam

import java.io.File

import htsjdk.samtools._
import nl.biopet.test.BiopetTest
import org.testng.annotations.{BeforeClass, Test}

/**
  * Created by wyleung on 22-2-16.
  * Create samfile and records are borrowed from WipeReadsTest by @bow
  */
class BamUtilsTest extends BiopetTest {

  private val samHeaderTemplate: SAMLineParser = {
    val samh = new SAMFileHeader
    samh.setSortOrder(SAMFileHeader.SortOrder.coordinate)
    samh.addSequence(new SAMSequenceRecord("chrQ", 10000))
    samh.addSequence(new SAMSequenceRecord("chrR", 10000))
    val readGroup1 = new SAMReadGroupRecord("001")
    readGroup1.setSample("sample01")
    samh.addReadGroup(readGroup1)
    val readGroup2 = new SAMReadGroupRecord("002")
    readGroup2.setSample("sample01")
    samh.addReadGroup(readGroup2)
    new SAMLineParser(samh)
  }

  private val samHeaderTemplateDoubleSample: SAMLineParser = {
    val samh = new SAMFileHeader
    samh.setSortOrder(SAMFileHeader.SortOrder.coordinate)
    samh.addSequence(new SAMSequenceRecord("chrQ", 10000))
    samh.addSequence(new SAMSequenceRecord("chrR", 10000))
    val readGroup1 = new SAMReadGroupRecord("001")
    readGroup1.setSample("sampleX")
    samh.addReadGroup(readGroup1)
    val readGroup2 = new SAMReadGroupRecord("002")
    readGroup2.setSample("sampleY")
    samh.addReadGroup(readGroup2)
    new SAMLineParser(samh)
  }

  private val samHeaderTemplateErrornousNoReadgroup: SAMLineParser = {
    val samh = new SAMFileHeader
    samh.setSortOrder(SAMFileHeader.SortOrder.coordinate)
    samh.addSequence(new SAMSequenceRecord("chrQ", 10000))
    samh.addSequence(new SAMSequenceRecord("chrR", 10000))
    new SAMLineParser(samh)
  }

  private def makeSams(header: SAMLineParser,
                       raws: Seq[String]): Seq[SAMRecord] =
    raws.map(s => header.parseLine(s))

  val sBamRecs1 = Seq(
    "r02\t0\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001",
    "r01\t16\tchrQ\t190\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001",
    "r01\t16\tchrQ\t290\t60\t10M\t*\t0\t0\tGGGGGAAAAA\tGGGGGGGGGG\tRG:Z:001",
    "r04\t0\tchrQ\t450\t60\t10M\t*\t0\t0\tCGTACGTACG\tEEFFGGHHII\tRG:Z:001",
    "r03\t16\tchrQ\t690\t60\t10M\t*\t0\t0\tCCCCCTTTTT\tHHHHHHHHHH\tRG:Z:001",
    "r05\t0\tchrQ\t890\t60\t5M200N5M\t*\t0\t0\tGATACGATAC\tFEFEFEFEFE\tRG:Z:001",
    "r06\t4\t*\t0\t0\t*\t*\t0\t0\tATATATATAT\tHIHIHIHIHI\tRG:Z:001"
  )

  val sBamRecs2 = Seq(
    "r02\t0\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\t",
    "r01\t16\tchrQ\t190\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\t"
  )

  val pBamRecs1 = Seq(
    "r02\t99\tchrQ\t50\t60\t10M\t=\t90\t50\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001",
    "r02\t147\tchrQ\t90\t60\t10M\t=\t50\t-50\tATGCATGCAT\tEEFFGGHHII\tRG:Z:001",
    "r01\t163\tchrQ\t150\t60\t10M\t=\t190\t50\tAAAAAGGGGG\tGGGGGGGGGG\tRG:Z:001",
    "r01\t83\tchrQ\t190\t60\t10M\t=\t150\t-50\tGGGGGAAAAA\tGGGGGGGGGG\tRG:Z:001",
    "r01\t163\tchrQ\t250\t60\t10M\t=\t290\t50\tAAAAAGGGGG\tGGGGGGGGGG\tRG:Z:001",
    "r01\t83\tchrQ\t290\t60\t10M\t=\t250\t-50\tGGGGGAAAAA\tGGGGGGGGGG\tRG:Z:001",
    "r04\t99\tchrQ\t450\t60\t10M\t=\t490\t50\tCGTACGTACG\tEEFFGGHHII\tRG:Z:001",
    "r04\t147\tchrQ\t490\t60\t10M\t=\t450\t-50\tGCATGCATGC\tEEFFGGHHII\tRG:Z:001",
    "r03\t163\tchrQ\t650\t60\t10M\t=\t690\t50\tTTTTTCCCCC\tHHHHHHHHHH\tRG:Z:001",
    "r03\t83\tchrQ\t690\t60\t10M\t=\t650\t-50\tCCCCCTTTTT\tHHHHHHHHHH\tRG:Z:001",
    "r05\t99\tchrQ\t890\t60\t5M200N5M\t=\t1140\t50\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001",
    "r05\t147\tchrQ\t1140\t60\t10M\t=\t890\t-50\tATGCATGCAT\tEEFFGGHHII\tRG:Z:001",
    "r06\t4\t*\t0\t0\t*\t*\t0\t0\tATATATATAT\tHIHIHIHIHI\tRG:Z:001",
    "r06\t4\t*\t0\t0\t*\t*\t0\t0\tGCGCGCGCGC\tHIHIHIHIHI\tRG:Z:001"
  )

  val pBamRecs2 = Seq(
    "r01\t163\tchrQ\t150\t60\t10M\t=\t190\t50\tAAAAAGGGGG\tGGGGGGGGGG\t",
    "r01\t83\tchrQ\t190\t60\t10M\t=\t150\t-50\tGGGGGAAAAA\tGGGGGGGGGG\t",
    "r01\t163\tchrQ\t250\t60\t10M\t=\t290\t50\tAAAAAGGGGG\tGGGGGGGGGG\t",
    "r01\t83\tchrQ\t290\t60\t10M\t=\t250\t-50\tGGGGGAAAAA\tGGGGGGGGGG\t"
  )

  @BeforeClass
  def start(): Unit = {
    createTestFileFrom(makeSams(samHeaderTemplate, sBamRecs1),
                       samHeaderTemplate,
                       BamUtilsTest.singleEndBam01)
    createTestFileFrom(makeSams(samHeaderTemplate, pBamRecs1),
                       samHeaderTemplate,
                       BamUtilsTest.pairedEndBam01)
    createTestFileFrom(makeSams(samHeaderTemplate, pBamRecs1),
                       samHeaderTemplate,
                       BamUtilsTest.pairedEndBam02)

    createTestFileFrom(makeSams(samHeaderTemplateDoubleSample, sBamRecs1),
                       samHeaderTemplateDoubleSample,
                       BamUtilsTest.singleEndBam01WithDoubleSamples)
    createTestFileFrom(makeSams(samHeaderTemplateDoubleSample, pBamRecs1),
                       samHeaderTemplateDoubleSample,
                       BamUtilsTest.pairedEndBam01WithDoubleSamples)

    createTestFileFrom(
      makeSams(samHeaderTemplateErrornousNoReadgroup, sBamRecs2),
      samHeaderTemplateErrornousNoReadgroup,
      BamUtilsTest.singleEndBam01NoRG)
    createTestFileFrom(
      makeSams(samHeaderTemplateErrornousNoReadgroup, pBamRecs2),
      samHeaderTemplateErrornousNoReadgroup,
      BamUtilsTest.pairedEndBam01NoRG)
  }

  private def createTestFileFrom(records: Seq[SAMRecord],
                                 header: SAMLineParser,
                                 output: File): File = {
    output.getParentFile.createNewFile()
    val outBam = new SAMFileWriterFactory()
      .setCreateIndex(true)
      .setUseAsyncIo(true)
      .makeBAMWriter(header.getFileHeader, true, output)
    writeBam(records, outBam)
    output
  }

  private def writeBam(records: Seq[SAMRecord], outBam: SAMFileWriter): Unit = {
    try {
      for (rec <- records) {
        outBam.addAlignment(rec)
      }
    } finally {
      outBam.close()
    }
  }

  @Test
  def testInputSingleEndOK(): Unit = {
    sBamRecs1.size shouldBe 7
  }

  @Test
  def testInputPairedEndOK(): Unit = {
    pBamRecs1.size shouldBe 14
  }

  @Test
  def testPairedRecords(): Unit = {
    sampleBamInsertSize(BamUtilsTest.pairedEndBam01) shouldBe 50
  }

  @Test
  def testContigInsertsize(): Unit = {
    contigInsertSize(BamUtilsTest.pairedEndBam01, "chrQ", 1, 10000) shouldBe Some(
      50)
    contigInsertSize(BamUtilsTest.pairedEndBam01, "chrR", 1, 10000) shouldBe None
  }

  @Test
  def testContigInsertsizeContigNotFound: Nothing = {
    intercept(contigInsertSize(BamUtilsTest.pairedEndBam01, "chrW", 1, 10000))

  }

  @Test
  def testSingleRecords(): Unit = {
    sampleBamInsertSize(BamUtilsTest.singleEndBam01) shouldBe 0
  }

  @Test
  def testMultiBamInsertsizes(): Unit = {
    val result = sampleBamsInsertSize(
      List(BamUtilsTest.singleEndBam01, BamUtilsTest.pairedEndBam01))
    result shouldEqual Map(
      BamUtilsTest.singleEndBam01 -> 0,
      BamUtilsTest.pairedEndBam01 -> 50
    )
  }

  @Test
  def testSampleBamNames(): Unit = {
    sampleBamMap(List(BamUtilsTest.singleEndBam01)) shouldEqual Map(
      "sample01" -> BamUtilsTest.singleEndBam01
    )
  }

  @Test
  def testSampleBamNamesMultipleSamplesSE(): Unit = {
    val thrown = intercept[IllegalArgumentException] {
      sampleBamMap(List(BamUtilsTest.singleEndBam01WithDoubleSamples))
    }
    thrown.getMessage should ===(
      "Bam contains multiple sample IDs: " + BamUtilsTest.singleEndBam01WithDoubleSamples)
  }

  @Test
  def testSampleBamNamesMultipleSamplesPE(): Unit = {
    val thrown = intercept[IllegalArgumentException] {
      sampleBamMap(List(BamUtilsTest.pairedEndBam01WithDoubleSamples))
    }
    thrown.getMessage should ===(
      "Bam contains multiple sample IDs: " + BamUtilsTest.pairedEndBam01WithDoubleSamples)
  }

  @Test
  def testSampleBamNamesNoRGSE(): Unit = {
    val thrown = intercept[IllegalArgumentException] {
      sampleBamMap(List(BamUtilsTest.singleEndBam01NoRG))
    }
    thrown.getMessage should ===(
      "Bam does not contain sample ID or have no readgroups defined: " + BamUtilsTest.singleEndBam01NoRG)
  }

  @Test
  def testSampleBamNamesNoRGPE(): Unit = {
    val thrown = intercept[IllegalArgumentException] {
      sampleBamMap(List(BamUtilsTest.pairedEndBam01NoRG))
    }
    thrown.getMessage should ===(
      "Bam does not contain sample ID or have no readgroups defined: " + BamUtilsTest.pairedEndBam01NoRG)
  }

  @Test
  def testSampleFoundTwice(): Unit = {
    val thrown = intercept[IllegalArgumentException] {
      sampleBamMap(
        List(BamUtilsTest.pairedEndBam01, BamUtilsTest.pairedEndBam02))
    }
    thrown.getMessage should ===("Samples has been found twice")
  }

  @Test
  def testReadgroup(): Unit = {
    val readgroups = sampleReadGroups(List(BamUtilsTest.pairedEndBam01))

    readgroups.headOption.map { case (x, _) => x } shouldBe Some("sample01")
    readgroups.values.map(_.size).sum shouldBe 2
  }

  @Test
  def testReadgroupReaders(): Unit = {
    val reader = SamReaderFactory.makeDefault.open(BamUtilsTest.pairedEndBam01)
    val readgroups = sampleReadGroups(
      Map("sample01" -> (reader, BamUtilsTest.pairedEndBam01)))

    readgroups.keys.headOption shouldBe Some("sample01")
    readgroups.values.map(_.size).sum shouldBe 2
  }

  @Test
  def testDictCheck(): Unit = {
    val dict1 =
      nl.biopet.utils.ngs.fasta.getCachedDict(resourceFile("/fake_chrQ.fa"))
    dict1.getSequences.size() shouldBe 1
    dict1.getSequence("chrQ").getSequenceLength shouldBe 16571

    val dict2 =
      nl.biopet.utils.ngs.fasta.getCachedDict(resourceFile("/fake_chrQ.fa"))
    dict1.hashCode() shouldBe dict2.hashCode()
    dict1.assertSameDictionary(dict2, true)
    dict1.assertSameDictionary(dict2, false)

    val dict3 = new SAMSequenceDictionary
    val dict4 = new SAMSequenceDictionary

    val seq1 = new SAMSequenceRecord("chr1", 4)
    val seq2 = new SAMSequenceRecord("chr5", 10)

    dict3.addSequence(seq1)
    dict3.addSequence(seq2)
    dict4.addSequence(seq2)
    dict4.addSequence(seq1)

    dict3.assertSameDictionary(dict4, true)
    intercept[AssertionError] {
      dict3.assertSameDictionary(dict4, false)
    }
  }

  @Test
  def testValidateReferenceInBamNotValid(): Unit = {
    intercept[AssertionError] {
      validateReferenceInBam(resourceFile("/paired01.bam"),
                             resourceFile("/fake_chrQ.fa"))
    }
  }

  @Test
  def testValidateReferenceInBamValid(): Unit = {
    validateReferenceInBam(resourceFile("/paired_valid.bam"),
                           resourceFile("/fake_chrQ.fa"))
  }
}

object BamUtilsTest {
  val singleEndBam01: File = File.createTempFile("bamutils", "single01.bam")
  singleEndBam01.deleteOnExit()
  val pairedEndBam01: File = File.createTempFile("bamutils", "paired01.bam")
  singleEndBam01.deleteOnExit()
  val pairedEndBam02: File = File.createTempFile("bamutils", "paired02.bam")
  singleEndBam01.deleteOnExit()

  val singleEndBam01WithDoubleSamples: File =
    File.createTempFile("bamutils", "single01ds.bam")
  singleEndBam01WithDoubleSamples.deleteOnExit()
  val pairedEndBam01WithDoubleSamples: File =
    File.createTempFile("bamutils", "paired01ds.bam")
  pairedEndBam01WithDoubleSamples.deleteOnExit()

  val singleEndBam01NoRG: File =
    File.createTempFile("bamutils", "single01norg.bam")
  singleEndBam01NoRG.deleteOnExit()
  val pairedEndBam01NoRG: File =
    File.createTempFile("bamutils", "paired01norg.bam")
  singleEndBam01NoRG.deleteOnExit()
}
