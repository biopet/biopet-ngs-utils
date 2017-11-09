package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.{VCFHeaderLineType, VCFInfoHeaderLine}
import nl.biopet.utils.{Histogram, Logging}

class InfoFieldHistogram(field: VCFInfoHeaderLine, method: FieldMethod.Value)
    extends Logging
    with Serializable {

  if (field.getType != VCFHeaderLineType.Integer && field.getType != VCFHeaderLineType.Float)
    logger.warn(s"Info field ${field.getID} is of type Integer or Float. " +
      s"Auto convert enabled but this may raise an exception when not possible")

  protected[InfoFieldHistogram] val counts = new Histogram[Double]()

  protected var _noValue = 0L
  def noValue: Long = _noValue
  protected var _total = 0L
  def total: Long = _total

  def addRecord(record: VariantContext): Unit = {
    val value = record.getAttAsDouble(field.getID, method)
    if (value.isEmpty) _noValue += 1
    else value.foreach(counts.add)
    _total += 1
  }

  def writeHistogram(outputFile: File): Unit = {
    counts.writeHistogramToTsv(outputFile)
  }

  def writeAggregateToTsv(outputFile: File): Unit = {
    counts.writeAggregateToTsv(outputFile)
  }

  def countsMap: Map[Double, Long] = counts.countsMap

  def aggregateStats: Map[String, Any] = counts.aggregateStats

  def +=(other: InfoFieldHistogram): InfoFieldHistogram = {
    this.counts += other.counts
    this
  }

}
