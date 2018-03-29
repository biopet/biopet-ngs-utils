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

package nl.biopet.utils.ngs.fasta

import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.intervals.BedRecord
import org.testng.annotations.Test

class ReferenceRegionTest extends BiopetTest {
  @Test
  def testDefault(): Unit = {
    val bedRecord = BedRecord("chrQ", 10, 20)
    val region = ReferenceRegion(resourceFile("/fake_chrQ.fa"), bedRecord)
    region.sequence.length shouldBe 10
    bedRecord.length shouldBe region.sequence.length
    region.sequenceString shouldBe "GAAACTCCAA"
  }

  @Test
  def testSubSequence(): Unit = {
    val bedRecord = BedRecord("chrQ", 10, 20)
    val region = ReferenceRegion(resourceFile("/fake_chrQ.fa"), bedRecord)
      .subSequence(12, 15)
    region.sequence.length shouldBe 4
    region.sequenceString shouldBe "AAAC"

    val region2 = ReferenceRegion(resourceFile("/fake_chrQ.fa"), bedRecord)
      .subSequence(13, 16)
    region2.sequence.length shouldBe 4
    region2.sequenceString shouldBe "AACT"
  }

  @Test
  def testNotExist(): Unit = {
    val bedRecord = BedRecord("chrQ", 10, 20)
    val region = ReferenceRegion(resourceFile("/fake_chrQ.fa"), bedRecord)

    intercept[IllegalArgumentException] {
      region.subSequence(10, 12)
    }.getMessage shouldBe "requirement failed: Start is not in sub sequence"

    intercept[IllegalArgumentException] {
      region.subSequence(11, 21)
    }.getMessage shouldBe "requirement failed: End is not in sub sequence"

    intercept[IllegalArgumentException] {
      region.subSequence(13, 12)
    }.getMessage shouldBe "requirement failed: Start is higher then end"

  }
}
