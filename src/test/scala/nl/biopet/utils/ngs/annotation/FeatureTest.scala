package nl.biopet.utils.ngs.annotation

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class FeatureTest extends BiopetTest {
  @Test
  def testGtfLine(): Unit = {

    intercept[IllegalArgumentException] {
      Feature.fromLine("line")
    }.getMessage shouldBe "requirement failed: A Gtf line should have 8 or 9 columns, gtf line: 'line'"

    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t3.0\t+\t.").score shouldBe Some(3.0)
    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t-\t.").score shouldBe None
    intercept[IllegalStateException] {
      Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\tnot_a_number\t-\t.")
    }.getMessage shouldBe "Score in a gtf line must be number"

    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t.").strand shouldBe Some(true)
    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t-\t.").strand shouldBe Some(false)
    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t.\t.").strand shouldBe None

    intercept[IllegalArgumentException] {
      Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t%\t.")
    }.getMessage should startWith("strand only allows '+' or '-', not %, gtf line: ")

    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t1").frame shouldBe Some('1')
    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t-\t2").frame shouldBe Some('2')
    Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t.\t.").frame shouldBe None

    intercept[IllegalArgumentException] {
      Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t.\tto_long")
    }.getMessage  should startWith("'to_long' can not be parsed as frame, gtf line:")


    val line = "chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t.\tgene_id \"ENSG00000223972.4\"; havana_gene \"OTTHUMG00000000961.2\"; gene_status \"KNOWN\"; gene_name \"DDX11L1\"; transcript_id \"ENSG00000223972.4\"; gene_type \"pseudogene\"; transcript_type \"pseudogene\"; transcript_name \"DDX11L1\"; transcript_status \"KNOWN\"; level \"2\""
    val feature = Feature.fromLine(line)

    feature.contig shouldBe "chr1"
    feature.source shouldBe "HAVANA"
    feature.feature shouldBe "gene"
    feature.start shouldBe 11869
    feature.end shouldBe 14412
    feature.attributes("gene_id") shouldBe "ENSG00000223972.4"
    feature.attributes("level") shouldBe "2"

    feature.asGtfLine shouldBe line

    val line2 = "chr1\tHAVANA\tpseudo_polyA\t134957\t134962\t.\t-\t.\tgene_id \"540278\"; gene_status \"NULL\"; gene_name \"540278\"; transcript_id \"540278\"; gene_type \"pseudo_polyA\"; transcript_type \"pseudo_polyA\"; transcript_name \"540278\"; transcript_status \"NULL\"; level \"2\""
    val feature2 = Feature.fromLine(line2)
    feature2.asGtfLine shouldBe line2
  }

  @Test
  def testGffLine(): Unit = {
    val line = "chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t.\tgene_id=ENSG00000223972.4;havana_gene=OTTHUMG00000000961.2;gene_status=KNOWN;gene_name=DDX11L1;transcript_id=ENSG00000223972.4;gene_type=pseudogene;transcript_type=pseudogene;transcript_name=DDX11L1;transcript_status=KNOWN;level=2"
    val feature = Feature.fromLine(line)

    feature.contig shouldBe "chr1"
    feature.source shouldBe "HAVANA"
    feature.feature shouldBe "gene"
    feature.start shouldBe 11869
    feature.end shouldBe 14412
    feature.attributes("gene_id") shouldBe "ENSG00000223972.4"
    feature.attributes("level") shouldBe "2"

    feature.asGff3Line shouldBe line

    val line2 = "chr1\tHAVANA\tpseudo_polyA\t134957\t134962\t.\t-\t.\tgene_id=540278;gene_status=NULL;gene_name=540278;transcript_id=540278;gene_type=pseudo_polyA;transcript_type=pseudo_polyA;transcript_name=540278;transcript_status=NULL;level=2"
    val feature2 = Feature.fromLine(line2)
    feature2.asGff3Line shouldBe line2
  }

  @Test
  def testPosition(): Unit = {
    val f1 = Feature.fromLine("chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t.")
    f1.minPosition shouldBe 11869
    f1.maxPosition shouldBe 14412

    val f2 = Feature.fromLine("chr1\tHAVANA\tgene\t14412\t11869\t.\t+\t.")
    f2.minPosition shouldBe 11869
    f2.maxPosition shouldBe 14412
  }
}
