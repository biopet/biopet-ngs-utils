/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
  val samples: Map[String, Int] =
    header.getSampleNameToOffset.toMap.map(x => x._1 -> x._2.toInt)

  protected[SampleCompare] val allelesCounts: Array[Array[Long]] =
    Array.fill(samples.size)(Array.fill(samples.size)(0L))
  protected[SampleCompare] val genotypesCounts: Array[Array[Long]] =
    Array.fill(samples.size)(Array.fill(samples.size)(0L))
  protected[SampleCompare] val refGenotypesCounts: Array[Array[Long]] =
    Array.fill(samples.size)(Array.fill(samples.size)(0L))
  protected[SampleCompare] val refAllelesCounts: Array[Array[Long]] =
    Array.fill(samples.size)(Array.fill(samples.size)(0L))

  /** Return the number of overlapping alleles */
  def allelesCount(name1: String)(name2: String): Long =
    allelesCounts(samples(name1))(samples(name2))

  /** Return the number of overlapping alleles */
  def allelesCount(id1: Int)(id2: Int): Long = allelesCounts(id1)(id2)

  /** Return the number of overlapping genotypes */
  def genotypesCount(name1: String)(name2: String): Long =
    genotypesCounts(samples(name1))(samples(name2))

  /** Return the number of overlapping genotypes */
  def genotypesCount(id1: Int)(id2: Int): Long = genotypesCounts(id1)(id2)

  /** This will combine 2 SampleCompare classes together */
  def +=(other: SampleCompare): SampleCompare = {
    require(this.samples == other.samples)
    for ((_, s1) <- samples; (_, s2) <- samples) {
      this.genotypesCounts(s1)(s2) += other.genotypesCounts(s1)(s2)
      this.allelesCounts(s1)(s2) += other.allelesCounts(s1)(s2)
      this.refGenotypesCounts(s1)(s2) += other.refGenotypesCounts(s1)(s2)
      this.refAllelesCounts(s1)(s2) += other.refAllelesCounts(s1)(s2)
    }
    this
  }

  /**
    * Checking a record to add to compate matrix
    * @param record VariantContext to add
    * @param sampleToSampleMinDepth If set a genotype need DP to be >=. This does depend on the DP fiald in the vcf record
    */
  def addRecord(record: VariantContext,
                sampleToSampleMinDepth: Option[Int]): Unit = {
    val compareSamples = sampleToSampleMinDepth
      .map { dp =>
        samples.filter(sample => record.getGenotype(sample._2).getDP >= dp)
      }
      .getOrElse(samples)

    val refIndex = record.getAlleleIndex(record.getReference)
    val alleles = compareSamples.map {
      case (_, i) =>
        i -> record
          .getGenotype(i)
          .getAlleles
          .map(record.getAlleleIndex)
          .toList
    }

    val refCounts = compareSamples.map {
      case (_, i) =>
        i -> alleles(i).count(_ == refIndex)
    }

    for ((_, sample1) <- compareSamples; (_, sample2) <- compareSamples) {
      val overlapRef = List(refCounts(sample1), refCounts(sample2)).min
      if (alleles(sample1) == alleles(sample2)) {
        genotypesCounts(sample1)(sample2) += 1
        if (alleles(sample1).forall(_ == refIndex))
          refGenotypesCounts(sample1)(sample2) += 1
        allelesCounts(sample1)(sample2) += alleles(sample1).size()
        refAllelesCounts(sample1)(sample2) += overlapRef
      } else {
        val c = alleleIndexOverlap(alleles(sample1), alleles(sample2))
        allelesCounts(sample1)(sample2) += c
        refAllelesCounts(sample1)(sample2) += overlapRef
      }
    }
  }

  /**
    * Writing a matrix to a file
    * @param counts Matrix to write
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  protected def writeOverlapFile(counts: Array[Array[Long]],
                                 outputFile: File,
                                 relative: Boolean = false): Unit = {
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
  def writeAlleleOverlap(outputFile: File, relative: Boolean = false): Unit =
    writeOverlapFile(allelesCounts, outputFile, relative)

  /**
    * Writes genotype matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeGenotypeOverlap(outputFile: File, relative: Boolean = false): Unit =
    writeOverlapFile(genotypesCounts, outputFile, relative)

  /**
    * Writes reference genotype matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeRefGenotypeOverlap(outputFile: File,
                              relative: Boolean = false): Unit =
    writeOverlapFile(refGenotypesCounts, outputFile, relative)

  /**
    * Writes reference genotype matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeRefAllelesOverlap(outputFile: File,
                             relative: Boolean = false): Unit =
    writeOverlapFile(refAllelesCounts, outputFile, relative)

  /**
    * Writes non reference genotype matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeNonRefGenotypeOverlap(outputFile: File,
                                 relative: Boolean = false): Unit = {
    writeOverlapFile(substractMatrix(genotypesCounts, refGenotypesCounts),
                     outputFile,
                     relative)
  }

  /**
    * Writes non reference allele matrix to a file
    * @param outputFile File to write to
    * @param relative If true values will be devided by the total number of counts for a sample
    */
  def writeNonRefAllelesOverlap(outputFile: File,
                                relative: Boolean = false): Unit = {
    writeOverlapFile(substractMatrix(allelesCounts, refAllelesCounts),
                     outputFile,
                     relative)
  }

  def substractMatrix(start: Array[Array[Long]],
                      substract: Array[Array[Long]]): Array[Array[Long]] = {
    start.zipWithIndex.map {
      case (row, idx1) =>
        row.zipWithIndex.map {
          case (cell, idx2) =>
            cell - substract(idx1)(idx2)
        }
    }
  }

  /**
    * Write all posible files to directory
    * @param outputDir Directory to write to
    */
  def writeAllFiles(outputDir: File): Unit = {
    require(outputDir.exists(), s"$outputDir does not exist")
    require(outputDir.isDirectory, s"$outputDir is not a directory")

    writeAlleleOverlap(new File(outputDir, "allele.abs.tsv"))
    writeAlleleOverlap(new File(outputDir, "allele.rel.tsv"), relative = true)
    writeGenotypeOverlap(new File(outputDir, "genotype.abs.tsv"))
    writeGenotypeOverlap(new File(outputDir, "genotype.rel.tsv"),
                         relative = true)
    writeRefGenotypeOverlap(new File(outputDir, "genotype.ref.abs.tsv"))
    writeRefAllelesOverlap(new File(outputDir, "allele.ref.abs.tsv"))
    writeNonRefGenotypeOverlap(new File(outputDir, "genotype.non_ref.abs.tsv"))
    writeNonRefAllelesOverlap(new File(outputDir, "allele.non_ref.abs.tsv"))
  }
}
