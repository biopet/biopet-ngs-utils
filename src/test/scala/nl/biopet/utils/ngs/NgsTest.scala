package nl.biopet.utils.ngs

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class NgsTest extends BiopetTest {
  @Test
  def testSeuqnce2bit(): Unit = {
    val seq = "ATCG"
    val i = sequenceTo2bitInt(seq)
    val newSeq = new String(int2bitToSequence(i))
    assert(newSeq.startsWith(seq))

    intercept[IllegalStateException] {
      sequenceTo2bitInt("N")
    }.getMessage shouldBe "Only ATCG is allowed here, 'N' is not"
  }
}
