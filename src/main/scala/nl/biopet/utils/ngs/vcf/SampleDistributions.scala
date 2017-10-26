package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import nl.biopet.utils.Histogram

import scala.collection.JavaConversions._

class SampleDistributions extends Serializable {

  /** Counts object to store results */
  protected[SampleDistributions] val counts: Map[GenotypeStats.Value, Histogram[Int]] =
    GenotypeStats.values.map(_  -> new Histogram[Int]).toMap

  def addRecord(record: VariantContext): Unit = {
    GenotypeStats.values.foreach { x =>
      counts(x).add(record.getGenotypes.count(x.method))
    }
  }

  /** Convert to immutable Map */
  def toMap: Map[GenotypeStats.Value, Map[Int, Long]] = counts.map(x => x._1 -> x._2.countsMap)

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
