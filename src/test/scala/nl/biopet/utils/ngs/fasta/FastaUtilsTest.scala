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
    val referenceFile: File = resourceFile("/fake_GRCh37.p13_no_alt_analysis_set.fa")

    val map = fasta.rebuildContigMap(inputFile, referenceFile)

    map("chr11_GL000202_random") shouldBe Set("HSCHR11_RANDOM_CTG2","GL000202.1", "NT_113921.2")
  }

  @Test
  def testRebuildContigMapCaseSensitive(): Unit = {
    val inputFile: File = resourceFile("/H.sapiens-GRCh37.contig.map.tsv")
    val referenceFile: File = resourceFile("/fake_GRCh37.p13_no_alt_analysis_set.fa")

    val map = fasta.rebuildContigMap(inputFile, referenceFile, caseSentive = true)

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
