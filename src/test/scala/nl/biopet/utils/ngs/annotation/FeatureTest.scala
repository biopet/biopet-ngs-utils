package nl.biopet.utils.ngs.annotation

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class FeatureTest extends BiopetTest {
  @Test
  def testGtfLine(): Unit = {
    val line = "chr1\tHAVANA\tgene\t11869\t14412\t.\t+\t.\tgene_id \"ENSG00000223972.4\"; transcript_id \"ENSG00000223972.4\"; gene_type \"pseudogene\"; gene_status \"KNOWN\"; gene_name \"DDX11L1\"; transcript_type \"pseudogene\"; transcript_status \"KNOWN\"; transcript_name \"DDX11L1\"; level 2; havana_gene \"OTTHUMG00000000961.2\";"
    val feature = Feature.fromLine(line)

    feature.contig shouldBe "chr1"
    feature.source shouldBe "HAVANA"
    feature.feature shouldBe "gene"
    feature.start shouldBe 11869
    feature.end shouldBe 14412
    feature.attributes("gene_id") shouldBe "ENSG00000223972.4"
    feature.attributes("level") shouldBe "2"

    val line2 = "chr1\tHAVANA\tpseudo_polyA\t134957\t134962\t.\t-\t.\tgene_id \"540278\"; transcript_id \"540278\"; gene_type \"pseudo_polyA\"; gene_status \"NULL\"; gene_name \"540278\"; transcript_type \"pseudo_polyA\"; transcript_status \"NULL\"; transcript_name \"540278\"; level 2; "
    val feature2 = Feature.fromLine(line2)

  }
}
