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

import java.io.{File, PrintWriter}

import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.fasta
import org.testng.annotations.Test

class FastaUtilsTest extends BiopetTest {

  @Test
  def testRebuildContigMap(): Unit = {
    val inputFile = File.createTempFile("contigMap.", ".tsv")
    inputFile.deleteOnExit()

    val writer = new PrintWriter(inputFile)
    writer.println("chrT\tchrQ")
    writer.close()

    val referenceFile = new File(resourcePath("/fake_chrQ.fa"))

    val map = fasta.rebuildContigMap(inputFile, referenceFile)

    map shouldBe Map("chrQ" -> Set("chrT"))
  }

  @Test
  def testRebuildContigMapReal(): Unit = {
    val inputFile: File = resourceFile("/H.sapiens-GRCh37.contig.map.tsv")
    val referenceFile: File = resourceFile(
      "/fake_GRCh37.p13_no_alt_analysis_set.fa")

    val map = fasta.rebuildContigMap(inputFile, referenceFile)

    map("chr11_GL000202_random") shouldBe Set("HSCHR11_RANDOM_CTG2",
                                              "GL000202.1",
                                              "NT_113921.2")
  }

  @Test
  def testRebuildContigMapCaseSensitive(): Unit = {
    val inputFile: File = resourceFile("/H.sapiens-GRCh37.contig.map.tsv")
    val referenceFile: File = resourceFile(
      "/fake_GRCh37.p13_no_alt_analysis_set.fa")

    val map =
      fasta.rebuildContigMap(inputFile, referenceFile, caseSentive = true)

    map("chr11_GL000202_random") shouldBe Set()
  }

  @Test
  def testReadContigMapReverse(): Unit = {
    val inputFile = File.createTempFile("contigMap.", ".tsv")
    inputFile.deleteOnExit()

    val writer = new PrintWriter(inputFile)
    writer.println("chrT\tchrQ")
    writer.close()

    val map = fasta.readContigMapReverse(inputFile)

    map shouldBe Map("chrQ" -> "chrT")
  }

  @Test
  def testGetSequenceGc(): Unit = {
    val referenceFile = new File(resourcePath("/fake_chrQ.fa"))

    fasta.getSequenceGc(referenceFile, "chrQ", 11, 20) shouldBe 0.4
  }

  @Test
  def testReadContigMap(): Unit = {
    val inputFile = File.createTempFile("contigMap.", ".tsv")
    inputFile.deleteOnExit()

    val writer = new PrintWriter(inputFile)
    writer.println("chrT\tchrQ")
    writer.println("chrX\t")
    writer.close()

    val map = fasta.readContigMap(inputFile)

    map shouldBe Map("chrT" -> Set("chrQ"), "chrX" -> Set())
  }
}
