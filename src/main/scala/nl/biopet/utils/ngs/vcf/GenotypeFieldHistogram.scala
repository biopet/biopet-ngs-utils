package nl.biopet.utils.ngs.vcf

import java.io.{File, PrintWriter}

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.{VCFFormatHeaderLine, VCFHeader, VCFHeaderLineType}
import nl.biopet.utils.{Histogram, Logging}

import scala.collection.JavaConversions._

class GenotypeFieldHistogram(header: VCFHeader,
                             field: VCFFormatHeaderLine,
                             method: FieldMethod.Value)
    extends Logging
    with Serializable {

  if (field.getType != VCFHeaderLineType.Integer && field.getType != VCFHeaderLineType.Float)
    logger.warn(s"Info field ${field.getID} is of type Integer or Float. " +
      s"Auto convert enabled but this may raise an exception when not possible")

  /** This map binds names to sample index */
  val samples: Map[String, Int] =
    header.getSampleNameToOffset.toMap.map(x => x._1 -> x._2.toInt)

  protected[GenotypeFieldHistogram] val counts: Map[Int, Histogram[Double]] =
    samples.map(_._2 -> new Histogram[Double]())

  protected var _noValue: Array[Long] = Array.fill(samples.size)(0L)
  def noValue: List[Long] = _noValue.toList
  protected var _total: Array[Long] = Array.fill(samples.size)(0L)
  def total: List[Long] = _total.toList

  def addRecord(record: VariantContext, sampleIdx: Option[Int] = None): Unit = {
    sampleIdx.map(_ :: Nil).getOrElse(samples.values).foreach { idx =>
      val value = record.getGenotype(idx).getAttAsDouble(field.getID, method)
      if (value.isEmpty) _noValue(idx) += 1
      else value.foreach(counts(idx).add)
      _total(idx) += 1
    }
  }

  def writeToFile(outputFile: File): Unit = {
    val writer = new PrintWriter(outputFile)
    writer.println(samples.keys.mkString("Sample\t", "\t", ""))
    val map = countsMap
    val values =
      map.foldLeft(Set[Double]())((a, b) => a ++ b._2.keys).toList.sorted
    for (value <- values) {
      writer.println(
        samples
          .map(s => map(s._2).getOrElse(value, 0L))
          .mkString(value + "\t", "\t", ""))
    }
    writer.close()
  }

  def countsMap: Map[Int, Map[Double, Long]] =
    counts.map(x => x._1 -> x._2.countsMap)

  def +=(other: GenotypeFieldHistogram): GenotypeFieldHistogram = {
    for ((idx, c) <- other.counts) this.counts(idx) += c
    this
  }

}
