package nl.biopet.utils.ngs.ped

import java.io.{File, PrintWriter}

import nl.biopet.utils.ngs.ped

import scala.io.Source

case class PedigreeFile(samples: List[PedigreeSample]) {
  def groupByFamilies: Map[String, List[PedigreeSample]] =
    samples.groupBy(_.familyId)

  def groupByPhenotype: Map[ped.Phenotype.Value, List[PedigreeSample]] =
    samples.groupBy(_.phenotype)

  def writeToFile(file: File): Unit = {
    val writer = new PrintWriter(file)
    samples.foreach(s => writer.println(s.toPedLine))
    writer.close()
  }
}

object PedigreeFile {
  def fromFile(file: File): PedigreeFile = {
    PedigreeFile(
      Source.fromFile(file).getLines().map(PedigreeSample.fromLine).toList)
  }
}
