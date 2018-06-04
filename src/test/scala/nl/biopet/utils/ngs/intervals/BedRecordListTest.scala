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

package nl.biopet.utils.ngs.intervals

import java.io.{File, PrintWriter}

import htsjdk.samtools.util.Interval
import org.scalatest.Matchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.{BeforeClass, Test}
import htsjdk.samtools.{SAMSequenceDictionary, SAMSequenceRecord}

/**
  * Created by pjvan_thof on 8/25/15.
  */
class BedRecordListTest extends TestNGSuite with Matchers {
  @BeforeClass
  def start(): Unit = {
    {
      val writer = new PrintWriter(BedRecordListTest.bedFile)
      writer.print(BedRecordListTest.bedContent)
      writer.close()
    }
    {
      val writer = new PrintWriter(BedRecordListTest.corruptBedFile)
      writer.print(BedRecordListTest.corruptBedContent)
      writer.close()
    }
    {
      val writer = new PrintWriter(BedRecordListTest.bedFileUcscHeader)
      writer.print(BedRecordListTest.ucscHeader)
      writer.print(BedRecordListTest.bedContent)
      writer.close()
    }
  }

  @Test
  def testReadBedFile() {
    val records = BedRecordList.fromFile(BedRecordListTest.bedFile)
    records.allRecords.size shouldBe 2
    records.header shouldBe Nil

    val tempFile = File.createTempFile("region", ".bed")
    tempFile.deleteOnExit()
    records.writeToFile(tempFile)
    BedRecordList.fromFile(tempFile) shouldBe records
    tempFile.delete()
  }

  @Test
  def testReadBedFileUcscHeader() {
    val records = BedRecordList.fromFile(BedRecordListTest.bedFileUcscHeader)
    records.allRecords.size shouldBe 2
    records.header shouldBe BedRecordListTest.ucscHeader.split("\n").toList

    val tempFile = File.createTempFile("region", ".bed")
    tempFile.deleteOnExit()
    records.writeToFile(tempFile)
    BedRecordList.fromFile(tempFile) shouldBe records
    tempFile.delete()
  }

  @Test def testSorted(): Unit = {
    val unsorted =
      BedRecordList.fromList(
        List(BedRecord("chrQ", 10, 20), BedRecord("chrQ", 0, 10)))
    unsorted.isSorted shouldBe false
    unsorted.sorted.isSorted shouldBe true
    val sorted = BedRecordList.fromList(
      List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 10, 20)))
    sorted.isSorted shouldBe true
    sorted.sorted.isSorted shouldBe true
    sorted.hashCode() shouldBe sorted.sorted.hashCode()
  }

  @Test def testOverlap(): Unit = {
    val list = BedRecordList.fromList(
      List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 10, 20)))
    list.overlapWith(BedRecord("chrQ", 5, 15)).size shouldBe 2
    list.overlapWith(BedRecord("chrQ", 0, 10)).size shouldBe 1
    list.overlapWith(BedRecord("chrQ", 10, 20)).size shouldBe 1
    list.overlapWith(BedRecord("chrQ", 19, 25)).size shouldBe 1
    list.overlapWith(BedRecord("chrQ", 20, 25)).size shouldBe 0
  }

  @Test def testLength(): Unit = {
    val list = BedRecordList.fromList(
      List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 10, 20)))
    list.length shouldBe 20
  }

  @Test def testCombineOverlap(): Unit = {
    val noOverlapList =
      BedRecordList.fromList(
        List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 10, 20)))
    noOverlapList.length shouldBe 20
    noOverlapList.combineOverlap.length shouldBe 20

    val overlapList = BedRecordList.fromList(
      List(BedRecord("chrQ", 0, 10),
           BedRecord("chrQ", 5, 15),
           BedRecord("chrQ", 10, 20)))
    overlapList.length shouldBe 30
    overlapList.combineOverlap.length shouldBe 20
  }

  @Test def testSquishBed(): Unit = {
    val noOverlapList =
      BedRecordList.fromList(
        List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 10, 20)))
    noOverlapList.length shouldBe 20
    noOverlapList.squishBed().length shouldBe 20

    val overlapList = BedRecordList.fromList(
      List(
        BedRecord("chrQ", 0, 10),
        BedRecord("chrQ", 5, 15),
        BedRecord("chrQ", 10, 20),
        BedRecord("chrQ", 25, 35),
        BedRecord("chrQ", 50, 80),
        BedRecord("chrQ", 60, 70)
      ))
    overlapList.length shouldBe 80
    val squishedList =
      overlapList.squishBed(strandSensitive = false, nameSensitive = false)
    squishedList.allRecords.size shouldBe 5
    squishedList.length shouldBe 40
  }

  @Test def testSamInterval(): Unit = {
    val list = BedRecordList.fromList(
      List(BedRecord("chrQ", 0, 10), BedRecord("chrQ", 5, 15)))
    list.toSamIntervals.toList shouldBe List(new Interval("chrQ", 1, 10),
                                             new Interval("chrQ", 6, 15))
  }

  @Test def testTraversable(): Unit = {
    val list = List(BedRecord("chrQ", 0, 10))
    BedRecordList.fromList(list) shouldBe BedRecordList.fromList(
      list.toIterator)
  }

  @Test def testErrors(): Unit = {
    intercept[IllegalStateException] {
      BedRecordList.fromFile(BedRecordListTest.corruptBedFile)
    }
  }

  @Test def testScatter(): Unit = {
    val list =
      BedRecordList.fromList(
        List(BedRecord("chrQ", 0, 1000),
             BedRecord("chrQ", 3000, 3500),
             BedRecord("chrQ", 3600, 3610),
             BedRecord("chrQ", 3610, 3620),
             BedRecord("chrQ", 3620, 3630)))
    val scatterList = list.scatter(100)
    scatterList.flatten.size shouldBe 18
    scatterList.size shouldBe 16
    scatterList.flatten.map(_.length).sum shouldBe 1530
  }

  @Test def testScatterContigOrder(): Unit = {
    val list = BedRecordList.fromList(
      List(BedRecord("chrA", 0, 200),
           BedRecord("chrB", 0, 95),
           BedRecord("chrC", 0, 100),
           BedRecord("chrD", 0, 45),
           BedRecord("chrE", 0, 45)))
    val dict = new SAMSequenceDictionary()
    dict.addSequence(new SAMSequenceRecord("chrA", 200))
    dict.addSequence(new SAMSequenceRecord("chrB", 95))
    dict.addSequence(new SAMSequenceRecord("chrC", 100))
    dict.addSequence(new SAMSequenceRecord("chrD", 45))
    dict.addSequence(new SAMSequenceRecord("chrE", 45))

    val scatter = list.scatter(100, sequenceDict = Option(dict))
    scatter.length shouldBe 5
    val flat = scatter.flatten
    flat.size shouldBe 6
    flat.headOption.map(_.chr) shouldBe Some("chrA")
    flat.lift(1).map(_.chr) shouldBe Some("chrA")
    flat.lift(2).map(_.chr) shouldBe Some("chrB")
    flat.lift(3).map(_.chr) shouldBe Some("chrC")
    flat.lift(4).map(_.chr) shouldBe Some("chrD")
    flat.lift(5).map(_.chr) shouldBe Some("chrE")
  }
}

object BedRecordListTest {
  val ucscHeader: String =
    """browser position chr7:127471196-127495720
                     |browser hide all
                     |track name="ItemRGBDemo" description="Item RGB demonstration" visibility=2 itemRgb="On"
                     |""".stripMargin
  val bedContent: String =
    """chr22	1000	5000	cloneA	960	+	1000	5000	0	2	567,488	0,3512
                  |chr22	2000	6000	cloneB	900	-	2000	6000	0	2	433,399	0,3601""".stripMargin
  val corruptBedContent: String =
    """chr22	5000	1000	cloneA	960	+	1000	5000	0	2	567,488	0,3512
                     |chr22	2000	6000	cloneB	900	-	2000	6000	0	2	433,399	0,3601""".stripMargin

  val bedFile: File = File.createTempFile("regions", ".bed")
  bedFile.deleteOnExit()
  val corruptBedFile: File = File.createTempFile("regions", ".bed")
  corruptBedFile.deleteOnExit()
  val bedFileUcscHeader: File = File.createTempFile("regions", ".bed")
  bedFileUcscHeader.deleteOnExit()
}
