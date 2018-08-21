package nl.biopet.utils.ngs.fastq

import htsjdk.samtools.fastq.FastqReader
import nl.biopet.test.BiopetTest
import scala.collection.JavaConversions._
class ValidationTest extends BiopetTest {

  def testValidPairs(): Unit = {
    val r1 = new FastqReader(resourceFile("/R1.fq"))
    val r2 = new FastqReader(resourceFile("/R2.fq"))
    val pairs = r1.iterator().toList.zip(r2.iterator().toList)
    pairs.foreach {case (r1, r2) => Validation.checkMate(r1, r2) shouldBe true}
  }
}
