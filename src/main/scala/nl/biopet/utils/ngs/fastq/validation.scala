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

import htsjdk.samtools.fastq.FastqRecord

package object validation {

  /**
    * This method checks if the pair is the same ID
    *
    * @param r1 R1 fastq record
    * @param r2 R2 fastq record
    * @return Boolean
    */
  def checkMate(r1: FastqRecord, r2: FastqRecord): Boolean = {
    // Take the read header before the space.
    // takeWhile is chosen because .split reads the entire string. This is faster.
    val id1 = r1.getReadName.takeWhile(_ != ' ')
    val id2 = r2.getReadName.takeWhile(_ != ' ')

    // true if IDs are equal, or if ids with /1, /2, .1 and .2 are equal.
    id1 == id2 ||
    id1.stripSuffix("/1") == id2.stripSuffix("/2") ||
    id1.stripSuffix(".1") == id2.stripSuffix(".2")
  }
}
