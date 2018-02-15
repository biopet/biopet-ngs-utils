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
import nl.biopet.utils.Counts

import scala.language.implicitConversions

/**
  * This class will collect general stats from vcf records
  */
class GeneralStats extends Serializable {

  /** Counts object to store results */
  protected[GeneralStats] val counts =
    new Counts[GeneralStats.Value](GeneralStats.values.map(_ -> 0L).toMap)

  /** Adding a [[VariantContext]] to the counts */
  def addRecord(record: VariantContext): Unit = {
    GeneralStats.values.filter(_.method(record)).foreach(counts.add)
  }

  /** Write results to a file */
  def writeToTsv(file: File): Unit = counts.writeHistogramToTsv(file)

  /** Convert to immutable Map */
  def toMap: Map[GeneralStats.Value, Long] = counts.countsMap

  /** Combine multiple classes into 1 */
  def +=(other: GeneralStats): GeneralStats = {
    this.counts += other.counts
    this
  }
}

/** Enum to store methods */
object GeneralStats extends Enumeration {
  protected case class Val(method: VariantContext => Boolean) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]

  val Total = Val(_ => true)
  val Biallelic = Val(_.isBiallelic)
  val ComplexIndel = Val(_.isComplexIndel)
  val Filtered = Val(_.isFiltered)
  val FullyDecoded = Val(_.isFullyDecoded)
  val Indel = Val(_.isIndel)
  val MNP = Val(_.isMNP)
  val MonomorphicInSamples = Val(_.isMonomorphicInSamples)
  val NotFiltered = Val(_.isNotFiltered)
  val PointEvent = Val(_.isPointEvent)
  val SimpleDeletion = Val(_.isSimpleDeletion)
  val SimpleInsertion = Val(_.isSimpleInsertion)
  val SNP = Val(_.isSNP)
  val StructuralIndel = Val(_.isStructuralIndel)
  val Symbolic = Val(_.isSymbolic)
  val SymbolicOrSV = Val(_.isSymbolicOrSV)
  val Variant = Val(_.isVariant)
}
