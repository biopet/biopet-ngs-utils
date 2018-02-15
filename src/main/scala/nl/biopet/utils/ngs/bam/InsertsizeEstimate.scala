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
  def regionsInsertSize(inputBam: File,
                        contig: String,
                        start: Int,
                        end: Int,
                        samplingSize: Int = 10000,
                        binSize: Int = 1000000): Option[Int] = {

    // create a bedList to divide the region into multiple pieces
    val insertSizesOnAllFragments = BedRecordList
      .fromList(Seq(BedRecord(contig, start, end)))
      .scatter(binSize)
      .flatten
      .par
      .flatMap({ bedRecord =>
        // for each scatter, open the bamFile for this specific region-query
        regionInsertSize(inputBam, bedRecord, samplingSize)
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

  def regionInsertSize(inputBam: File,
                       bedRecord: BedRecord,
                       samplingSize: Int = 10000): Option[Int] = {
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
  }
}
