package nl.biopet.utils.ngs.vcf

import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFInfoHeaderLine
import nl.biopet.utils.Histogram

class InfoFieldHistogram(field: VCFInfoHeaderLine, method: FieldMethod.Value) {

  protected[InfoFieldHistogram] val counts = new Histogram[Double]()

  protected var _noValue = 0L
  def noValue: Long = _noValue
  protected var _total = 0L
  def total: Long = _total

  def addRecord(record: VariantContext): Unit = {
    val value = record.getAttAsDouble(field.getID, method)
    value.foreach(counts.add)
    if (value.isEmpty) _noValue += 1
    _total += 1
  }

  def writeHistogram(outputFile: File): Unit = {
    counts.writeAggregateToTsv(outputFile)
  }

  def +=(other: InfoFieldHistogram): InfoFieldHistogram = {
    this.counts += other.counts
    this
  }

}
