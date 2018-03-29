package nl.biopet.utils.ngs.fasta

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import nl.biopet.utils.ngs.intervals.BedRecord

case class ReferenceRegion(sequence: Array[Byte], offset: Int)

object ReferenceRegion {
  def apply(fastaFile: IndexedFastaSequenceFile,
            contig: String,
            start: Int,
            end: Int): ReferenceRegion = {
    ReferenceRegion(fastaFile.getSubsequenceAt(contig, start, end).getBases,
                    start)
  }

  def apply(fastaFile: IndexedFastaSequenceFile,
            region: BedRecord): ReferenceRegion = {
    ReferenceRegion(fastaFile, region.chr, region.start, region.end)
  }

}
