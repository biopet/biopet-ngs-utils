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

package nl.biopet.utils.ngs.fastq.validation
import htsjdk.samtools.fastq.FastqRecord
import nl.biopet.test.BiopetTest
import org.testng.annotations.{DataProvider, Test}

class ValidationTest extends BiopetTest {

  @DataProvider(name = "validPairs")
  def provider(): Array[Array[Any]] =
    Array(
      Array(
        new FastqRecord(
          "@SEQ_ID 1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_ID 2",
          "ATGC",
          "+",
          "AA!!"
        )
      ),
      Array(
        new FastqRecord(
          "@SEQ_ID/1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_ID/2",
          "ATGC",
          "+",
          "AA!!"
        )
      ),
      Array(
        new FastqRecord(
          "@SEQ_ID.1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_ID.2",
          "ATGC",
          "+",
          "AA!!"
        )
      )
    )

  @DataProvider(name = "invalidPairs")
  def invalidPairs(): Array[Array[Any]] =
    Array(
      Array(
        new FastqRecord(
          "@SEQ_ID 1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_IDASD 2",
          "ATGC",
          "+",
          "AA!!"
        )
      ),
      Array(
        new FastqRecord(
          "@SEQ_ID/1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_ID/3",
          "ATGC",
          "+",
          "AA!!"
        )
      ),
      Array(
        new FastqRecord(
          "@SEQ_ID.1",
          "ATGC",
          "+",
          "AA!!"
        ),
        new FastqRecord(
          "@SEQ_ID..2",
          "ATGC",
          "+",
          "AA!!"
        )
      )
    )

  @Test(dataProvider = "validPairs")
  def testValidPairs(r1: FastqRecord, r2: FastqRecord): Unit = {
    checkMate(r1, r2) shouldBe true
  }

  @Test(dataProvider = "invalidPairs")
  def testInvalidPairs(r1: FastqRecord, r2: FastqRecord): Unit = {
    checkMate(r1, r2) shouldBe false
  }

}
