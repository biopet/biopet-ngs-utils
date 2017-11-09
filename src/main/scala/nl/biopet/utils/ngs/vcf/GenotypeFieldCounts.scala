package nl.biopet.utils.ngs.vcf

import java.io.{File, PrintWriter}

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.{VCFFormatHeaderLine, VCFHeader}
import nl.biopet.utils.Counts

import scala.collection.JavaConversions._

class GenotypeFieldCounts(header: VCFHeader, field: VCFFormatHeaderLine, method: FieldMethod.Value) {

  /** This map binds names to sample index */
  val samples: Map[String, Int] = header.getSampleNameToOffset.toMap.map(x => x._1 -> x._2.toInt)

  protected[GenotypeFieldCounts] val counts: Map[Int, Counts[String]] = samples.map(_._2 -> new Counts[String]())

  protected var _noValue: Array[Long] = Array.fill(samples.size)(0L)
  def noValue: List[Long] = _noValue.toList
  protected var _total: Array[Long] = Array.fill(samples.size)(0L)
  def total: List[Long] = _total.toList

  def addRecord(record: VariantContext, sampleIdx: Option[Int] = None): Unit = {
    sampleIdx.map(_ :: Nil).getOrElse(samples.values).foreach { idx =>
      val value = record.getGenotype(idx).getAttAsString(field.getID, method)
      if (value.isEmpty) _noValue(idx) += 1
      else value.foreach(counts(idx).add)
      _total(idx) += 1
    }
  }

  def writeToFile(outputFile: File): Unit = {
    val writer = new PrintWriter(outputFile)
    writer.println(samples.keys.mkString("Sample\t", "\t", ""))
    val map = countsMap
    val values = map.foldLeft(Set[String]())((a,b) => a ++ b._2.keys).toList.sorted
    for (value <- values) {
      writer.println(samples.map(s => map(s._2).getOrElse(value, 0L)).mkString(value + "\t", "\t", ""))
    }
    writer.close()
  }

  def countsMap: Map[Int, Map[String, Long]] = counts.map(x => x._1 -> x._2.countsMap)

  def +=(other: GenotypeFieldCounts): GenotypeFieldCounts = {
    for ((idx, c) <- other.counts) this.counts(idx) += c
    this
  }

}
