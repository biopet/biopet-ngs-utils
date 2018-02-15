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

case class PedigreeSample(familyId: String,
                          sampleId: String,
                          paternalId: String,
                          maternalId: String,
                          gender: Gender.Value,
                          phenotype: Phenotype.Value) {
  def toPedLine: String =
    List(familyId, sampleId, paternalId, maternalId, gender.id, phenotype.id)
      .mkString("\t")
}

object PedigreeSample {

  private lazy val regex = "[\\t ]".r

  def fromLine(line: String): PedigreeSample = {
    val values = regex.split(line)
    val gender: Gender.Value = values(4) match {
      case "1" => Gender.Male
      case "2" => Gender.Female
      case _   => Gender.Unknown
    }
    val phenotype: Phenotype.Value = values(5) match {
      case "1" => Phenotype.Unaffected
      case "2" => Phenotype.Affected
      case _   => Phenotype.Missing

    }
    PedigreeSample(values(0),
                   values(1),
                   values(2),
                   values(3),
                   gender,
                   phenotype)
  }
}
