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

package nl.biopet.utils.ngs.ped

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class PedigreeSampleTest extends BiopetTest {
  @Test
  def testFromLine(): Unit = {
    val v1 = List("a", "b", "c", "d", "1", "1")
    PedigreeSample.fromLine(v1.mkString("\t")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Male,
      Phenotype.Unaffected)
    PedigreeSample.fromLine(v1.mkString(" ")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Male,
      Phenotype.Unaffected)

    val v2 = List("a", "b", "c", "d", "2", "2")
    PedigreeSample.fromLine(v2.mkString("\t")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Female,
      Phenotype.Affected)
    PedigreeSample.fromLine(v2.mkString(" ")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Female,
      Phenotype.Affected)

    val v3 = List("a", "b", "c", "d", "0", "0")
    PedigreeSample.fromLine(v3.mkString("\t")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Unknown,
      Phenotype.Missing)
    PedigreeSample.fromLine(v3.mkString(" ")) shouldBe PedigreeSample(
      "a",
      "b",
      "c",
      "d",
      Gender.Unknown,
      Phenotype.Missing)
  }

  @Test
  def testToPedLine(): Unit = {
    PedigreeSample("a", "b", "c", "d", Gender.Male, Phenotype.Unaffected).toPedLine shouldBe List(
      "a",
      "b",
      "c",
      "d",
      "1",
      "1").mkString("\t")
    PedigreeSample("a", "b", "c", "d", Gender.Female, Phenotype.Affected).toPedLine shouldBe List(
      "a",
      "b",
      "c",
      "d",
      "2",
      "2").mkString("\t")
    PedigreeSample("a", "b", "c", "d", Gender.Unknown, Phenotype.Missing).toPedLine shouldBe List(
      "a",
      "b",
      "c",
      "d",
      "0",
      "0").mkString("\t")
  }
}
