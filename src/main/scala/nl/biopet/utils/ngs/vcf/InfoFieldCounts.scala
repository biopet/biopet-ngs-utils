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

import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFInfoHeaderLine
import nl.biopet.utils.{Counts, Histogram}

/** This class will track all possible value of a single INFO field */
class InfoFieldCounts(field: VCFInfoHeaderLine, method: FieldMethod.Value)
    extends Serializable {

  protected[InfoFieldCounts] val counts = new Counts[String]()

  protected[InfoFieldCounts] var _noValue = 0L
  protected[InfoFieldCounts] var _total = 0L

  /** Return number of records that does not have this field */
  def noValue: Long = _noValue

  /** Return total number of records */
  def total: Long = _total

  /** Add record to counts */
  def addRecord(record: VariantContext): Unit = {
    val value = record.getAttAsString(field.getID, method)
    if (value.isEmpty) _noValue += 1
    else value.foreach(counts.add)
    _total += 1
  }

  /**
    * Write histogram to a file
    * @param outputFile File to write to
    */
  def writeHistogram(outputFile: File): Unit = {
    counts.writeHistogramToTsv(outputFile)
  }

  /** Convert strings to doubles if possible */
  def asHistrogram: Histogram[Double] = {
    new Histogram[Double](counts.countsMap.map {
      case (k, v) => k.toDouble -> v
    })
  }

  /** Returns a map of the counts */
  def countsMap: Map[String, Long] = counts.countsMap

  /** Combining an other [[InfoFieldCounts]] into this */
  def +=(other: InfoFieldCounts): InfoFieldCounts = {
    this.counts += other.counts
    this._noValue += other._noValue
    this._total += other._total
    this
  }

}
