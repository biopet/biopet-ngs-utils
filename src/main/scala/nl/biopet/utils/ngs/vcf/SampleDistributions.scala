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
import nl.biopet.utils.Histogram

import scala.collection.JavaConversions._

class SampleDistributions extends Serializable {

  /** Counts object to store results */
  protected[SampleDistributions] val counts
    : Map[GenotypeStats.Value, Histogram[Int]] =
    GenotypeStats.values.map(_ -> new Histogram[Int]).toMap

  def addRecord(record: VariantContext): Unit = {
    GenotypeStats.values.foreach { x =>
      counts(x).add(record.getGenotypes.count(x.method))
    }
  }

  /** Convert to immutable Map */
  def toMap: Map[GenotypeStats.Value, Map[Int, Long]] =
    counts.map(x => x._1 -> x._2.countsMap)

  /** Write results to a directory */
  def writeToDir(outputDir: File): Unit = {
    GenotypeStats.values.foreach { x =>
      counts(x).writeHistogramToTsv(new File(outputDir, s"$x.tsv"))
      counts(x).writeAggregateToTsv(new File(outputDir, s"$x.aggregate.tsv"))
    }
  }

  /** Combine with an other [[SampleDistributions]] */
  def +=(other: SampleDistributions): SampleDistributions = {
    GenotypeStats.values.foreach(x => this.counts(x) += other.counts(x))
    this
  }
}
