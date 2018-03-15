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

package nl.biopet.utils.ngs.fastq

case class Encoding(name: String, min: Char, max: Char)

object Encoding {

  def sanger = Encoding("Sanger", '!', 'I')
  def solexa = Encoding("Solexa", ';', 'h')
  def illumina13 = Encoding("Illumina 1.3+", '@', 'h')
  def illumina15 = Encoding("Illumina 1.5+", 'C', 'h')
  def illumina18 = Encoding("Illumina 1.8+", '!', 'J')

  def allEncodings: List[Encoding] = List(
    sanger,
    solexa,
    illumina13,
    illumina15,
    illumina18
  )

  def possibleEncodings(min: Char, max: Char): List[Encoding] = {
    allEncodings.filter(e => min >= e.min && max <= e.max)
  }
}
