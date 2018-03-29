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

package nl.biopet.utils.ngs

import nl.biopet.test.BiopetTest
import org.testng.annotations.{DataProvider, Test}

class NgsTest extends BiopetTest {

  @DataProvider(name = "seq")
  def sequenceProvider: Array[Array[Any]] = {
    Array(
      Array("ATCG", Some((0 << 0) + (1 << 2) + (2 << 4) + (3 << 6))),
      Array("ATCGATCGATCGATCG", None)
    )
  }

  @Test(dataProvider = "seq")
  def testSingleSequence2bit(seq: String, result: Option[Int]): Unit = {
    val i = sequenceTo2bitSingleInt(seq)
    result.foreach(i shouldBe _)
    val newSeq = new String(int2bitToSequence(i))
    assert(newSeq.startsWith(seq))
  }

  @Test(dataProvider = "seq")
  def testSequence2bit(seq: String, result: Option[Int]): Unit = {
    val i = sequenceTo2bitInt(seq)
    val newSeq = new String(int2bitToSequence(i))
    assert(newSeq.startsWith(seq))
  }

  @Test
  def testSingleSequenceWrongChar(): Unit = {
    intercept[IllegalStateException] {
      sequenceTo2bitSingleInt("N")
    }.getMessage shouldBe "Only ATCG is allowed here, 'N' is not"

    intercept[IllegalStateException] {
      sequenceTo2bitInt("N")
    }.getMessage shouldBe "Only ATCG is allowed here, 'N' is not"
  }
}
