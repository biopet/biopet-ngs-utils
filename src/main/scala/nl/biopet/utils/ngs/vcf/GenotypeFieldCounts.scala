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
import htsjdk.variant.vcf.{VCFFormatHeaderLine, VCFHeader}
import nl.biopet.utils.Counts

import scala.collection.JavaConversions._

/** This class will track all values of 1 genotype field */
class GenotypeFieldCounts(header: VCFHeader,
                          field: VCFFormatHeaderLine,
                          method: FieldMethod.Value)
    extends Serializable {

  /** This map binds names to sample index */
  val samples: Map[String, Int] =
    header.getSampleNameToOffset.toMap.map { case (k, v) => k -> v.toInt }

  protected[GenotypeFieldCounts] val counts: Map[Int, Counts[String]] =
    samples.map { case (_, v) => v -> new Counts[String]() }

  protected val _noValue: Array[Long] = Array.fill(samples.size)(0L)
  protected val _total: Array[Long] = Array.fill(samples.size)(0L)

  /** Returns per sample the number of records without the field */
  def noValue: Map[String, Long] = samples.map {
    case (k, v) => k -> _noValue(v)
  }

  /** Returns per sample the number of total records */
  def total: Map[String, Long] = samples.map { case (k, v) => k -> _total(v) }

  /**
    * Add records to counts
    * @param record Vcf record
    * @param sampleIdx Optional: Only do this given sample
    */
  def addRecord(record: VariantContext, sampleIdx: Option[Int] = None): Unit = {
    sampleIdx.map(_ :: Nil).getOrElse(samples.values).foreach { idx =>
      val value = record
        .getGenotype(idx)
        .getAttAsString(field.getID, method)
      if (value.isEmpty) _noValue(idx) += 1
      else value.foreach(counts(idx).add)
      _total(idx) += 1
    }
  }

  /** Write histograms to a single file */
  def writeToFile(outputFile: File): Unit = {
    val writer = new PrintWriter(outputFile)
    val sampleNames = samples.keys.toList.sorted
    writer.println(sampleNames.mkString("Sample\t", "\t", ""))
    val map = countsMap
    val values =
      map
        .foldLeft(Set[String]()) { case (a, (_, b)) => a ++ b.keys }
        .toList
        .sorted
    for (value <- values) {
      val line = sampleNames
        .map(s => map(s).getOrElse(value, 0L))
        .mkString(value + "\t", "\t", "")
      writer.println(line)
    }
    writer.close()
  }

  /** Return a map of counts */
  def countsMap: Map[String, Map[String, Long]] =
    samples.map { case (k, v) => k -> counts(v).countsMap }

  def +=(other: GenotypeFieldCounts): GenotypeFieldCounts = {
    for ((idx, c) <- other.counts) {
      this.counts(idx) += c
      this._noValue(idx) += other._noValue(idx)
      this._total(idx) += other._total(idx)
    }
    this
  }
}
