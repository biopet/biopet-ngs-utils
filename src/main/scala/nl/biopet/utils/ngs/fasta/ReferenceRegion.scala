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
