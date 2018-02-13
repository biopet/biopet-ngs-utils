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
    header.getSampleNameToOffset.toMap.map(x => x._1 -> x._2.toInt)

  protected[GenotypeFieldCounts] val counts: Map[Int, Counts[String]] =
    samples.map(_._2 -> new Counts[String]())

  protected val _noValue: Array[Long] = Array.fill(samples.size)(0L)
  protected val _total: Array[Long] = Array.fill(samples.size)(0L)

  /** Returns per sample the number of records without the field */
  def noValue: Map[String, Long] = samples.map(x => x._1 -> _noValue(x._2))

  /** Returns per sample the number of total records */
  def total: Map[String, Long] = samples.map(x => x._1 -> _total(x._2))

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
    writer.println(samples.keys.mkString("Sample\t", "\t", ""))
    val map = countsMap
    val values =
      map.foldLeft(Set[String]())((a, b) => a ++ b._2.keys).toList.sorted
    for (value <- values) {
      writer.println(
        samples
          .map(s => map(s._1).getOrElse(value, 0L))
          .mkString(value + "\t", "\t", ""))
    }
    writer.close()
  }

  /** Return a map of counts */
  def countsMap: Map[String, Map[String, Long]] =
    samples.map(x => x._1 -> counts(x._2).countsMap)

  def +=(other: GenotypeFieldCounts): GenotypeFieldCounts = {
    for ((idx, c) <- other.counts) {
      this.counts(idx) += c
      this._noValue(idx) += other._noValue(idx)
      this._total(idx) += other._total(idx)
    }
    this
  }
}
