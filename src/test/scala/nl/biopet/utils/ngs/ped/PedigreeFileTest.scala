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

import java.io.File

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class PedigreeFileTest extends BiopetTest {

  def samples = List(
    PedigreeSample("f1", "s1", "s2", "s3", Gender.Male, Phenotype.Unaffected),
    PedigreeSample("f1", "s2", "", "", Gender.Male, Phenotype.Affected),
    PedigreeSample("f1", "s3", "", "", Gender.Male, Phenotype.Unaffected),
    PedigreeSample("f2", "s4", "", "", Gender.Male, Phenotype.Missing)
  )

  @Test
  def testFile(): Unit = {
    val pedFile = new PedigreeFile(samples)
    val file = File.createTempFile("test.", ".ped")
    file.deleteOnExit()
    pedFile.writeToFile(file)

    pedFile("s4") shouldBe PedigreeSample("f2",
                                          "s4",
                                          "",
                                          "",
                                          Gender.Male,
                                          Phenotype.Missing)

    val pedFile2 = PedigreeFile.fromFile(file)
    pedFile shouldBe pedFile2
  }

  @Test
  def testFiles(): Unit = {
    val pedFile1 = new PedigreeFile(samples)
    val pedFile2 = new PedigreeFile(
      PedigreeSample("f2", "s5", "", "", Gender.Male, Phenotype.Missing) :: Nil)

    val combinedPedFile = pedFile1 + pedFile2
    pedFile1.samples.size shouldBe 4
    pedFile2.samples.size shouldBe 1
    combinedPedFile.samples.size shouldBe 5
  }

  @Test
  def testGroupByFamily(): Unit = {
    val pedFile = new PedigreeFile(samples)
    val families = pedFile.groupByFamilies
    families.keySet shouldBe Set("f1", "f2")

    families("f1") shouldBe samples.filter(_.familyId == "f1")
    families("f2") shouldBe samples.filter(_.familyId == "f2")
  }

  @Test
  def testGroupByPhenotype(): Unit = {
    val pedFile = new PedigreeFile(samples)
    val families = pedFile.groupByPhenotype
    families.keySet shouldBe Set(Phenotype.Unaffected,
                                 Phenotype.Affected,
                                 Phenotype.Missing)

    families(Phenotype.Unaffected) shouldBe samples.filter(
      _.phenotype == Phenotype.Unaffected)
    families(Phenotype.Affected) shouldBe samples.filter(
      _.phenotype == Phenotype.Affected)
    families(Phenotype.Missing) shouldBe samples.filter(
      _.phenotype == Phenotype.Missing)
  }
}
