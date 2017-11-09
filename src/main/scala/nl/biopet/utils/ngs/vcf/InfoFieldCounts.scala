package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFInfoHeaderLine
import nl.biopet.utils.{Counts, Histogram}

class InfoFieldCounts(field: VCFInfoHeaderLine, method: FieldMethod.Value) {

  protected[InfoFieldCounts] val counts = new Counts[String]()

  protected var _noValue = 0L
  def noValue: Long = _noValue
  protected var _total = 0L
  def total: Long = _total

  def addRecord(record: VariantContext): Unit = {
    val value = record.getAttAsString(field.getID, method)
    if (value.isEmpty) _noValue += 1
    else value.foreach(counts.add)
    _total += 1
  }

  def writeHistogram(outputFile: File): Unit = {
    counts.writeHistogramToTsv(outputFile)
  }

  def countsMap: Map[String, Long] = counts.countsMap

  def +=(other: InfoFieldCounts): InfoFieldCounts = {
    this.counts += other.counts
    this
  }

}
