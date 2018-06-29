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

import htsjdk.samtools.util.Interval
import org.scalatest.Matchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by pjvanthof on 24/08/15.
  */
class BedRecordTest extends TestNGSuite with Matchers {
  @Test def testLineParse(): Unit = {
    BedRecord("chrQ", 0, 4) shouldBe BedRecord("chrQ", 0, 4)
    BedRecord.fromLine("chrQ\t0\t4") shouldBe BedRecord("chrQ", 0, 4)
    BedRecord.fromLine("chrQ\t0\t4\tname\t3\t+") shouldBe BedRecord(
      "chrQ",
      0,
      4,
      Some("name"),
      Some(3.0),
      Some(true))
    BedRecord.fromLine("chrQ\t0\t4\tname\t3\t+\t1\t3") shouldBe
      BedRecord("chrQ",
                0,
                4,
                Some("name"),
                Some(3.0),
                Some(true),
                Some(1),
                Some(3))
    BedRecord.fromLine("chrQ\t0\t4\tname\t3\t+\t1\t3\t255,0,0") shouldBe
      BedRecord("chrQ",
                0,
                4,
                Some("name"),
                Some(3.0),
                Some(true),
                Some(1),
                Some(3),
                Some((255, 0, 0)))
    BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t20,50") shouldBe
      BedRecord("chrQ",
                0,
                100,
                Some("name"),
                Some(3.0),
                Some(true),
                Some(1),
                Some(3),
                Some((255, 0, 0)),
                Some(2),
                IndexedSeq(10, 20),
                IndexedSeq(20, 50))
  }

  @Test def testLineOutput(): Unit = {
    BedRecord("chrQ", 0, 4).toString shouldBe "chrQ\t0\t4"
    BedRecord.fromLine("chrQ\t0\t4").toString shouldBe "chrQ\t0\t4"
    BedRecord
      .fromLine("chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t20,50")
      .toString shouldBe "chrQ\t0\t100\tname\t3.0\t+\t1\t3\t255,0,0\t2\t10,20\t20,50"
  }

  @Test def testOverlap(): Unit = {
    BedRecord("chrQ", 0, 4).overlapWith(BedRecord("chrQ", 0, 4)) shouldBe true
    BedRecord("chrQ", 0, 4).overlapWith(BedRecord("chrX", 0, 4)) shouldBe false
    BedRecord("chrQ", 0, 4).overlapWith(BedRecord("chrQ", 4, 8)) shouldBe false
    BedRecord("chrQ", 0, 4).overlapWith(BedRecord("chrQ", 3, 8)) shouldBe true
    BedRecord("chrQ", 4, 8).overlapWith(BedRecord("chrQ", 0, 4)) shouldBe false
    BedRecord("chrQ", 3, 4).overlapWith(BedRecord("chrQ", 0, 4)) shouldBe true
    BedRecord("chrQ", 3, 4).overlapWith(BedRecord("chrQ", 4, 5)) shouldBe false
  }

  @Test def testLength(): Unit = {
    BedRecord("chrQ", 0, 4).length shouldBe 4
    BedRecord("chrQ", 0, 1).length shouldBe 1
    BedRecord("chrQ", 3, 4).length shouldBe 1
  }

  @Test def testToSamInterval(): Unit = {
    BedRecord("chrQ", 0, 4).toSamInterval shouldBe new Interval("chrQ", 1, 4)
    BedRecord("chrQ", 0, 4, Some("name"), Some(0.0), Some(true)).toSamInterval shouldBe new Interval(
      "chrQ",
      1,
      4,
      false,
      "name")
  }

  @Test def testExons(): Unit = {
    BedRecord
      .fromLine("chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0")
      .exons shouldBe None

    val record = BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t0,80")
    record.exons should not be None
    record.exons match {
      case Some(exons) =>
        exons(0).originals().headOption shouldBe Some(record)
        exons(0).originals().size shouldBe 1
        exons(1).originals().headOption shouldBe Some(record)
        exons(1).originals().size shouldBe 1
        exons(0).start shouldBe 0
        exons(0).end shouldBe 10
        exons(1).start shouldBe 80
        exons(1).end shouldBe 100
        exons.foldLeft(0)(_ + _.length) shouldBe 30
      case _ =>
    }
  }

  @Test def testIntrons(): Unit = {
    BedRecord
      .fromLine("chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0")
      .introns shouldBe None

    val record = BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t0,80")
    record.introns should not be None
    record.introns match {
      case Some(introns) =>
        introns(0).originals().headOption shouldBe Some(record)
        introns(0).originals().size shouldBe 1
        introns(0).start shouldBe 10
        introns(0).end shouldBe 80
        introns.foldLeft(0)(_ + _.length) shouldBe 70
      case _ =>
    }
  }

  @Test def testExonIntronOverlap(): Unit = {
    val record = BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t0,80")
    val exons = record.exons
    val introns = record.introns
    for (exon <- exons.toList.flatten; intron <- introns.toList.flatten) {
      exon.overlapWith(intron) shouldBe false
    }
  }

  @Test def testUtrsPositive(): Unit = {
    BedRecord.fromLine("chrQ\t0\t100\tname\t3\t+").utr3 shouldBe None
    BedRecord.fromLine("chrQ\t0\t100\tname\t3\t+").utr5 shouldBe None

    val record = BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t+\t3\t93\t255,0,0\t2\t10,20\t0,80")
    val utr5 = record.utr5
    val utr3 = record.utr3
    utr5 should not be None
    utr3 should not be None
    utr5.map(_.length) shouldBe Some(3)
    utr3.map(_.length) shouldBe Some(7)

  }

  @Test def testUtrsNegative(): Unit = {
    BedRecord.fromLine("chrQ\t0\t100\tname\t3\t-").utr3 shouldBe None
    BedRecord.fromLine("chrQ\t0\t100\tname\t3\t-").utr5 shouldBe None

    val record = BedRecord.fromLine(
      "chrQ\t0\t100\tname\t3\t-\t3\t93\t255,0,0\t2\t10,20\t0,80")
    val utr5 = record.utr5
    val utr3 = record.utr3
    utr5 should not be None
    utr3 should not be None
    utr5.map(_.length) shouldBe Some(7)
    utr3.map(_.length) shouldBe Some(3)
  }

  @Test def testOriginals(): Unit = {
    val original = BedRecord("chrQ", 1, 2)
    val level1 = BedRecord("chrQ", 1, 2, _originals = List(original))
    val level2 = BedRecord("chrQ", 2, 3, _originals = List(level1))
    original.originals() shouldBe List(original)
    original.originals(nested = false) shouldBe List(original)
    level1.originals() shouldBe List(original)
    level1.originals(nested = false) shouldBe List(original)
    level2.originals() shouldBe List(original)
    level2.originals(nested = false) shouldBe List(level1)
  }

  @Test def testScatter(): Unit = {
    val list = BedRecord("chrQ", 0, 1000).scatter(10)
    list.size shouldBe 100
    BedRecordList.fromList(list).length shouldBe 1000
    for (l1 <- list; l2 <- list if l1 != l2) l1.overlapWith(l2) shouldBe false

    val list2 = BedRecord("chrQ", 0, 999).scatter(10)
    list2.size shouldBe 99
    BedRecordList.fromList(list2).length shouldBe 999
    for (l1 <- list2; l2 <- list2 if l1 != l2) l1.overlapWith(l2) shouldBe false

    val list3 = BedRecord("chrQ", 0, 999).scatter(9)
    list3.size shouldBe 111
    BedRecordList.fromList(list3).length shouldBe 999
    for (l1 <- list3; l2 <- list3 if l1 != l2) l1.overlapWith(l2) shouldBe false
  }

  @Test def testErrors(): Unit = {
    BedRecord("chrQ", 0, 3).validate
    BedRecord
      .fromLine("chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10,20\t20,50")
      .validate
    intercept[IllegalArgumentException] {
      BedRecord("chrQ", 0, 0).validate
    }
    intercept[IllegalArgumentException] {
      BedRecord("chrQ", 4, 3).validate
    }
    intercept[IllegalArgumentException] {
      BedRecord
        .fromLine("chrQ\t0\t100\tname\t3\t+\t1\t3\t255,0,0\t2\t10\t50")
        .validate
    }
    intercept[IllegalStateException] {
      BedRecord
        .fromLine("chrQ\t0\t100\tname\t3\tx\t1\t3\t255,0,0\t2\t10,20\t20,50")
        .validate
    }
  }
}
