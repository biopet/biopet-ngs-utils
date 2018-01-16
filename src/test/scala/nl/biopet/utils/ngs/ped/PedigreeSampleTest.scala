package nl.biopet.utils.ngs.ped

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class PedigreeSampleTest extends BiopetTest {
  @Test
  def testFromLine(): Unit = {
    val v1 = List("a", "b", "c", "d", "1", "1")
    PedigreeSample.fromLine(v1.mkString("\t")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Male, Phenotype.Unaffected)
    PedigreeSample.fromLine(v1.mkString(" ")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Male, Phenotype.Unaffected)

    val v2 = List("a", "b", "c", "d", "2", "2")
    PedigreeSample.fromLine(v2.mkString("\t")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Female, Phenotype.Affected)
    PedigreeSample.fromLine(v2.mkString(" ")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Female, Phenotype.Affected)

    val v3 = List("a", "b", "c", "d", "0", "0")
    PedigreeSample.fromLine(v3.mkString("\t")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Unknown, Phenotype.Missing)
    PedigreeSample.fromLine(v3.mkString(" ")) shouldBe PedigreeSample("a", "b", "c", "d", Gender.Unknown, Phenotype.Missing)
  }

  @Test
  def testToPedLine(): Unit = {
    PedigreeSample("a", "b", "c", "d", Gender.Male, Phenotype.Unaffected).toPedLine shouldBe List("a", "b", "c", "d", "1", "1").mkString("\t")
    PedigreeSample("a", "b", "c", "d", Gender.Female, Phenotype.Affected).toPedLine shouldBe List("a", "b", "c", "d", "2", "2").mkString("\t")
    PedigreeSample("a", "b", "c", "d", Gender.Unknown, Phenotype.Missing).toPedLine shouldBe List("a", "b", "c", "d", "0", "0").mkString("\t")
  }
}
