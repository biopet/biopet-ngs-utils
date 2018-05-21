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

package nl.biopet.utils.ngs.fasta

import java.io.File

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import nl.biopet.utils.ngs.intervals.BedRecord

/**
  * This class can store a part of a contig and can still do queries relative to the original contig.
  * This is useful when doing a lot of small queries
  */
case class ReferenceRegion(sequence: Array[Byte],
                           contig: String,
                           start: Int,
                           end: Int) {
  def sequenceString: String = new String(sequence)

  /** Extract sub sequence, 1-based positions */
  def subSequence(start: Int, end: Int): ReferenceRegion = {
    require(start >= this.start, "Start is not in sub sequence")
    require(end <= this.end, "End is not in sub sequence")
    require(start <= end, "Start is higher then end")
    val newSeq =
      sequence.slice(start - this.start, end - this.end + this.start - 1)
    ReferenceRegion(newSeq, contig, start, end)
  }
}

object ReferenceRegion {
  def apply(fastaFile: IndexedFastaSequenceFile,
            contig: String,
            start: Int,
            end: Int): ReferenceRegion = {
    ReferenceRegion(fastaFile.getSubsequenceAt(contig, start, end).getBases,
                    contig,
                    start,
                    end)
  }

  def apply(fastaFile: File,
            contig: String,
            start: Int,
            end: Int): ReferenceRegion = {
    val reader = new IndexedFastaSequenceFile(fastaFile)
    ReferenceRegion(reader, contig, start, end)
  }

  def apply(fastaFile: IndexedFastaSequenceFile,
            region: BedRecord): ReferenceRegion = {
    ReferenceRegion(fastaFile, region.chr, region.start + 1, region.end)
  }

  def apply(fastaFile: File, region: BedRecord): ReferenceRegion = {
    val reader = new IndexedFastaSequenceFile(fastaFile)
    ReferenceRegion(reader, region)
  }
}
