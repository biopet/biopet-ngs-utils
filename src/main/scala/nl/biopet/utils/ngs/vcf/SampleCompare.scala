package nl.biopet.utils.ngs.vcf

import java.io.{File, PrintWriter}

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFHeader

import scala.collection.JavaConversions._

/**
  * This class can record overlapping alleles and genotypes
  *
  * @param header header of the vcf file, this is used to retrieve the sample indexes
  */
class SampleCompare(header: VCFHeader) extends Serializable {
  /** This map binds names to sample index */
  val samples: Map[String, Int] = header.getSampleNameToOffset.toMap.map(x => x._1 -> x._2.toInt)

  protected[SampleCompare] val allelesCounts: Array[Array[Long]] = Array.fill(samples.size)(Array.fill(samples.size)(0L))
  protected[SampleCompare] val genotypesCounts: Array[Array[Long]] = Array.fill(samples.size)(Array.fill(samples.size)(0L))

  /** Return the number of overlapping alleles */
  def allelesCount(name1: String)(name2: String): Long = allelesCounts(samples(name1))(samples(name2))

  /** Return the number of overlapping alleles */
  def allelesCount(id1: Int)(id2: Int): Long = allelesCounts(id1)(id2)

  /** Return the number of overlapping genotypes */
  def genotypesCount(name1: String)(name2: String): Long = genotypesCounts(samples(name1))(samples(name2))

  /** Return the number of overlapping genotypes */
  def genotypesCount(id1: Int)(id2: Int): Long = genotypesCounts(id1)(id2)

  /** This will combine 2 SampleCompare classes together */
  def +=(other: SampleCompare): SampleCompare = {
    require(this.samples == other.samples)
    for ((_, s1) <- samples; (_, s2) <- samples) {
      this.genotypesCounts(s1)(s2) += other.genotypesCounts(s1)(s2)
      this.allelesCounts(s1)(s2) += other.allelesCounts(s1)(s2)
    }
    this
  }

  /**
    * Checking a record to add to compate matrix
    * @param record VariantContext to add
    * @param sampleToSampleMinDepth If set a genotype need DP to be >=. This does depend on the DP fiald in the vcf record
    */
  def addRecord(record: VariantContext, sampleToSampleMinDepth: Option[Int]): Unit = {
    val compareSamples = sampleToSampleMinDepth.map { dp =>
      samples.filter(sample => record.getGenotype(sample._2).getDP >= dp)
    }.getOrElse(samples)

    val alleles = compareSamples.map(s => s._2 -> record.getGenotype(s._2).getAlleles.map(record.getAlleleIndex).toList)

    for (sample1 <- compareSamples; sample2 <- compareSamples) {
      if (alleles(sample1._2) == alleles(sample2._2)) {
        genotypesCounts(sample1._2)(sample2._2) += 1
        allelesCounts(sample1._2)(sample2._2) += alleles(sample1._2).size()
      } else {
        allelesCounts(sample1._2)(sample2._2) += alleleIndexOverlap(alleles(sample1._2), alleles(sample2._2))
      }
    }
  }

  /**
    * Writing a matrix to a file
    * @param counts Matrix to write
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  protected def writeOverlapFile(counts: Array[Array[Long]], outputFile: File, relative: Boolean = false): Unit = {
    val writer = new PrintWriter(outputFile)
    val sorted = samples.toList.sortBy(_._1)
    writer.print("Sample")
    writer.println("\t" + sorted.map(_._1).mkString("\t"))
    for ((name, idx) <- sorted) {
      writer.print(name)
      val values = sorted.map(idx2 => counts(idx)(idx2._2))
      if (relative) {
        val total = counts(idx)(idx).toDouble
        writer.println("\t" + values.map(_.toDouble / total).mkString("\t"))
      } else {
        writer.println("\t" + values.mkString("\t"))
      }
    }
    writer.close()
  }

  /**
    * Writer allele matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeAlleleOverlap(outputFile: File, relative: Boolean = false): Unit = writeOverlapFile(allelesCounts, outputFile, relative)

  /**
    * Writes genotype matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeGenotypeOverlap(outputFile: File, relative: Boolean = false): Unit = writeOverlapFile(genotypesCounts, outputFile, relative)

  /**
    * Write all posible files to directory
    * @param outputDir Directory to write to
    */
  def writeAllFiles(outputDir: File): Unit = {
    require(outputDir.exists(), s"$outputDir doe not exist")
    require(outputDir.isDirectory, s"$outputDir is not a directory")

    writeAlleleOverlap(new File(outputDir, "allele.abs.tsv"))
    writeAlleleOverlap(new File(outputDir, "allele.rel.tsv"), relative = true)
    writeGenotypeOverlap(new File(outputDir, "genotype.abs.tsv"))
    writeGenotypeOverlap(new File(outputDir, "genotype.rel.tsv"), relative = true)
  }

}

