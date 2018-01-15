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

    pedFile("s4") shouldBe PedigreeSample("f2", "s4", "", "", Gender.Male, Phenotype.Missing)

    val pedFile2 = PedigreeFile.fromFile(file)
    pedFile shouldBe pedFile2
  }

  @Test
  def testFiles(): Unit = {
    val pedFile1 = new PedigreeFile(samples)
    val pedFile2 = new PedigreeFile(PedigreeSample("f2", "s5", "", "", Gender.Male, Phenotype.Missing) :: Nil)

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
    families.keySet shouldBe Set(Phenotype.Unaffected, Phenotype.Affected, Phenotype.Missing)

    families(Phenotype.Unaffected) shouldBe samples.filter(_.phenotype == Phenotype.Unaffected)
    families(Phenotype.Affected) shouldBe samples.filter(_.phenotype == Phenotype.Affected)
    families(Phenotype.Missing) shouldBe samples.filter(_.phenotype == Phenotype.Missing)
  }
}
