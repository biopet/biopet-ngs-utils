package nl.biopet.utils.ngs.ped

import java.io.{File, PrintWriter}

import nl.biopet.utils.ngs.ped

import scala.io.Source

case class PedigreeFile(samples: Map[String, PedigreeSample]) {
  def this(s: List[PedigreeSample]) {
    this(s.map(sample => sample.sampleId -> sample).toMap)
  }

  lazy val groupByFamilies: Map[String, List[PedigreeSample]] =
    samples.values.toList.groupBy(_.familyId)

  lazy val groupByPhenotype: Map[ped.Phenotype.Value, List[PedigreeSample]] =
    samples.values.toList.groupBy(_.phenotype)

  def writeToFile(file: File): Unit = {
    val writer = new PrintWriter(file)
    samples.values.foreach(s => writer.println(s.toPedLine))
    writer.close()
  }

  def apply(id: String) = samples(id)

  def +(other: PedigreeFile) = new PedigreeFile(this.samples ++ other.samples)
}

object PedigreeFile {
  def fromFile(file: File): PedigreeFile = {
    new PedigreeFile(
      Source.fromFile(file).getLines().map(PedigreeSample.fromLine).toList)
  }
}
