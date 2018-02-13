package nl.biopet.utils.ngs.bam

import java.io.File

import htsjdk.samtools.{SamReader, SamReaderFactory}
import nl.biopet.utils.ngs.intervals.{BedRecord, BedRecordList}

import scala.collection.mutable

object InsertsizeEstimate {

  /**
    * Estimate the insertsize of fragments within the given contig.
    * Uses the properly paired reads according to flags set by the aligner
    *
    * @param inputBam input bam file
    * @param contig contig to scan for
    * @param end position to stop scanning
    * @return Int with insertsize for this contig
    */
  def contigInsertSize(inputBam: File,
                       contig: String,
                       start: Int,
                       end: Int,
                       samplingSize: Int = 10000,
                       binSize: Int = 1000000): Option[Int] = {

    // create a bedList to devide the contig into multiple pieces
    val insertSizesOnAllFragments = BedRecordList
      .fromList(Seq(BedRecord(contig, start, end)))
      .scatter(binSize)
      .flatten
      .par
      .flatMap({ bedRecord =>
        // for each scatter, open the bamfile for this specific region-query
        val inputSam: SamReader = SamReaderFactory.makeDefault.open(inputBam)
        val samIterator =
          inputSam.query(bedRecord.chr, bedRecord.start, bedRecord.end, true)

        val counts: mutable.Map[Int, Int] = mutable.Map()

        for (_ <- 0 until samplingSize if samIterator.hasNext) {
          val rec = samIterator.next()
          val isPaired = rec.getReadPairedFlag
          val minQ10 = rec.getMappingQuality >= 10
          val pairOnSameContig = rec.getContig == rec.getMateReferenceName

          if (isPaired && minQ10 && pairOnSameContig) {
            val insertSize = rec.getInferredInsertSize.abs
            counts(insertSize) = counts.getOrElse(insertSize, 0) + 1
          }
        }

        counts.keys.size match {
          case 1 => Some(counts.keys.head)
          case 0 => None
          case _ =>
            Some(counts.foldLeft(0)((old, observation) => {
              observation match {
                case (insertSize: Int, observations: Int) =>
                  (old + (insertSize * observations)) / (observations + 1)
                case _ => 0
              }
            }))
        }
      })

    insertSizesOnAllFragments.size match {
      case 1 => Some(insertSizesOnAllFragments.head)
      case 0 => None
      case _ =>
        Some(insertSizesOnAllFragments.foldLeft(0)((old, observation) => {
          (old + observation) / 2
        }))

    }
  }
}
