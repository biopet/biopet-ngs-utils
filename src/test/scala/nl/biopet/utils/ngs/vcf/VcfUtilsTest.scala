/**
  * Biopet is built on top of GATK Queue for building bioinformatic
  * pipelines. It is mainly intended to support LUMC SHARK cluster which is running
  * SGE. But other types of HPC that are supported by GATK Queue (such as PBS)
  * should also be able to execute Biopet tools and pipelines.
  *
  * Copyright 2014 Sequencing Analysis Support Core - Leiden University Medical Center
  *
  * Contact us at: sasc@lumc.nl
  *
  * A dual licensing mode is applied. The source code within this project is freely available for non-commercial use under an AGPL
  * license; For commercial users or users who do not want to follow the AGPL
  * license, please contact us to obtain a separate license.
  */
package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.variantcontext.{Allele, GenotypeBuilder, VariantContextBuilder}
import htsjdk.variant.vcf.{VCFHeader, VCFHeaderLine}
import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.intervals.BedRecord
import org.testng.annotations.Test

import scala.collection.JavaConversions._

/**
  * @author Peter van 't Hof
  * @author Sander Bollen
  */
class VcfUtilsTest extends BiopetTest {
  
  @Test
  def testCompoundNoCall(): Unit = {
    val noAllele = Allele.NO_CALL
    val refAllele = Allele.create("A", true)
    val compoundNoCall = GenotypeBuilder.create("sample_01", List(noAllele, refAllele))
    compoundNoCall.isCompoundNoCall shouldBe true

    val altAllele = Allele.create("G", false)
    val normalGenotype = GenotypeBuilder.create("sample_01", List(refAllele, altAllele))
    normalGenotype.isCompoundNoCall shouldBe false

    val completeNoCall = GenotypeBuilder.create("sample_01", List(noAllele, noAllele))
    completeNoCall.isCompoundNoCall shouldBe false
  }

  @Test
  def testAlleleOverlap(): Unit = {

    val a1 = Allele.create("G")
    val a2 = Allele.create("A")

    alleleOverlap(List(a1, a1), List(a1, a1)) shouldBe 2
    alleleOverlap(List(a2, a2), List(a2, a2)) shouldBe 2
    alleleOverlap(List(a1, a2), List(a1, a2)) shouldBe 2
    alleleOverlap(List(a1, a2), List(a2, a1)) shouldBe 2
    alleleOverlap(List(a2, a1), List(a1, a2)) shouldBe 2
    alleleOverlap(List(a2, a1), List(a2, a1)) shouldBe 2

    alleleOverlap(List(a1, a2), List(a1, a1)) shouldBe 1
    alleleOverlap(List(a2, a1), List(a1, a1)) shouldBe 1
    alleleOverlap(List(a1, a1), List(a1, a2)) shouldBe 1
    alleleOverlap(List(a1, a1), List(a2, a1)) shouldBe 1

    alleleOverlap(List(a1, a1), List(a2, a2)) shouldBe 0
    alleleOverlap(List(a2, a2), List(a1, a1)) shouldBe 0
  }

  @Test
  def testFillAllele(): Unit = {
    fillAllele("A", 4) shouldBe "A---"
    fillAllele("A", 1) shouldBe "A"
    fillAllele("A", 2, '+') shouldBe "A+"
  }

  @Test
  def testLoadRegion(): Unit = {
    loadRegion(resourceFile("/multi.vcf.gz"), BedRecord("chrQ", 0, 4)).size shouldBe 4
    loadRegion(resourceFile("/multi.vcf.gz"), BedRecord("chrQ", 0, 2)).size shouldBe 2
    loadRegion(resourceFile("/multi.vcf.gz"), BedRecord("chrQ", 1, 3)).size shouldBe 2
  }

  @Test
  def testLoadRegions(): Unit = {
    val it = loadRegions(resourceFile("/multi.vcf.gz"), List(BedRecord("chrQ", 0, 2), BedRecord("chrQ", 2, 4)).toIterator)
    it.size shouldBe 4
    it.close()

    loadRegions(resourceFile("/multi.vcf.gz"), List(BedRecord("chrQ", 0, 1), BedRecord("chrQ", 3, 4)).toIterator)
      .size shouldBe 2
  }

  @Test
  def testGetSampleIds(): Unit = {
    getSampleIds(resourceFile("/multi.vcf.gz")) shouldBe List("Sample_1", "Sample_2", "Sample_3")
  }

  @Test
  def testGetVcfIndexFile(): Unit = {
    getVcfIndexFile(new File("test.vcf")) shouldBe new File("test.vcf.idx")
    getVcfIndexFile(new File("test.vcf.gz")) shouldBe new File("test.vcf.gz.tbi")
    intercept[IllegalArgumentException] {
      getVcfIndexFile(new File("test.txt"))
    }.getMessage shouldBe "File given is no vcf file: test.txt"
  }

  @Test
  def testVcfFileIsEmpty(): Unit = {
    vcfFileIsEmpty(resourceFile("/multi.vcf.gz")) shouldBe false
    vcfFileIsEmpty(resourceFile("/empty.vcf")) shouldBe true
  }

  @Test
  def testIdenticalGenotype(): Unit = {
    val g1 = GenotypeBuilder.create("test", List(Allele.create("A"), Allele.create("A")))
    g1.identicalGenotype(g1) shouldBe true

    val g2 = GenotypeBuilder.create("test", List(Allele.create("A"), Allele.create("C")))
    g2.identicalGenotype(g2) shouldBe true
    g1.identicalGenotype(g2) shouldBe false

    val g3 = GenotypeBuilder.create("test2", List(Allele.create("A"), Allele.create("A")))
    g3.identicalGenotype(g3) shouldBe true
    g1.identicalGenotype(g3) shouldBe false
  }

  @Test
  def testIdenticalVariantContext(): Unit = {
    val alleles1 = List(Allele.create("A", true), Allele.create("C"))
    val alleles2 = List(Allele.create("A", true), Allele.create("G"))
    val alleles3 = List(Allele.create("AGC", true), Allele.create("G"))
    val alleles4 = List(Allele.create("A", true), Allele.create("C"), Allele.create("G"))

    val v1 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles1, 2).alleles(alleles1).make()
    v1.identicalVariantContext(v1) shouldBe true

    val v2 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles2, 2).alleles(alleles2).make()
    v2.identicalVariantContext(v2) shouldBe true
    v1.identicalVariantContext(v2) shouldBe false

    val v3 = new VariantContextBuilder().chr("chr1").start(2).computeEndFromAlleles(alleles1, 2).alleles(alleles1).make()
    v3.identicalVariantContext(v3) shouldBe true
    v1.identicalVariantContext(v3) shouldBe false

    val v4 = new VariantContextBuilder().chr("chr1").start(3).computeEndFromAlleles(alleles1, 3).alleles(alleles1).make()
    v4.identicalVariantContext(v4) shouldBe true
    v3.identicalVariantContext(v4) shouldBe false

    val v5 = new VariantContextBuilder().chr("chr1").start(3).computeEndFromAlleles(alleles3, 3).alleles(alleles3).make()
    v5.identicalVariantContext(v5) shouldBe true
    v4.identicalVariantContext(v5) shouldBe false

    val g1 = GenotypeBuilder.create("test", alleles1)
    val g2 = GenotypeBuilder.create("test", alleles2)

    val v6 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles4, 2).alleles(alleles4).genotypes(g1).make()
    val v7 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles4, 2).alleles(alleles4).genotypes(g2).make()
    v6.identicalVariantContext(v6) shouldBe true
    v7.identicalVariantContext(v7) shouldBe true
    v6.identicalVariantContext(v7) shouldBe false
  }

  @Test
  def testGetLongestAlleleSize(): Unit = {
    val alleles1 = List(Allele.create("A", true), Allele.create("G"))
    val alleles2 = List(Allele.create("AGC", true), Allele.create("G"))

    val v1 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles1, 2).alleles(alleles1).make()
    v1.getLongestAlleleSize shouldBe 1

    val v2 = new VariantContextBuilder().chr("chrQ").start(2).computeEndFromAlleles(alleles2, 2).alleles(alleles2).make()
    v2.getLongestAlleleSize shouldBe 3
  }

  @Test
  def testHasMinGenomeQuality(): Unit = {
    val alleles1 = List(Allele.create("A", true), Allele.create("C"))
    val g1 = GenotypeBuilder.create("test", alleles1)

    g1.hasGQ shouldBe false
    g1.hasMinGenomeQuality(3) shouldBe false
    g1.hasMinGenomeQuality(0) shouldBe false

    val g2 = new GenotypeBuilder().GQ(3).alleles(alleles1).name("test").make()

    g2.hasGQ shouldBe true
    g2.hasMinGenomeQuality(2) shouldBe true
    g2.hasMinGenomeQuality(3) shouldBe true
    g2.hasMinGenomeQuality(4) shouldBe false
  }

  @Test
  def testIsBlockGVcf(): Unit = {
    val h1 = new VCFHeader
    isBlockGVcf(h1) shouldBe false

    val h2 = new VCFHeader
    h2.addMetaDataLine(new VCFHeaderLine("GVCFBlock", ""))
    isBlockGVcf(h2) shouldBe true
  }
}
