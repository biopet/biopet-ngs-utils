package nl.biopet.utils.ngs.vcf

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class VcfFieldTest extends BiopetTest {
  @Test
  def testCorrect(): Unit = {
    val vcfField = VcfField.fromArg("DP:All")
    vcfField.toString shouldBe "DP_All"
    vcfField.key shouldBe "DP"
    vcfField.method shouldBe FieldMethod.All
  }

  @Test
  def testInCorrect(): Unit = {
    intercept[IllegalArgumentException] {
      VcfField.fromArg("DP:All:Extra")
    }.getMessage should startWith(
      "requirement failed: A field should be formatted like: <tag>:<method>. Possible methods: ")

    intercept[IllegalArgumentException] {
      VcfField.fromArg("DP:Something_else")
    }.getMessage should startWith(
      "Method 'Something_else' does not exis. Possible methods: ")
  }
}
