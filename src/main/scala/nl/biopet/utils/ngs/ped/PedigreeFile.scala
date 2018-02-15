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

package nl.biopet.utils.ngs.ped

import java.io.{File, PrintWriter}

import nl.biopet.utils.ngs.ped

import scala.io.Source

case class PedigreeFile(samples: Map[String, PedigreeSample]) {
  def this(s: List[PedigreeSample]) {
    this(s.map(sample => sample.sampleId -> sample).toMap)
  }

  def groupByFamilies: Map[String, List[PedigreeSample]] =
    samples.values.toList.groupBy(_.familyId)

  def groupByPhenotype: Map[ped.Phenotype.Value, List[PedigreeSample]] =
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
