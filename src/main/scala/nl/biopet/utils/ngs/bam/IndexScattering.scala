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

import htsjdk.samtools.{BAMIndex, SAMSequenceDictionary, SamReaderFactory}
import nl.biopet.utils.ngs.intervals.{BedRecord, BedRecordList}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

object IndexScattering {
  def createBamBinsReference(
      bamFile: File,
      chunks: Int,
      mixContigs: Boolean = true): List[List[BedRecord]] = {
    val samReader = SamReaderFactory.makeDefault().open(bamFile)
    val dict = samReader.getFileHeader.getSequenceDictionary
    samReader.close()
    createBamBins(BedRecordList.fromDict(dict).allRecords.toList,
                  bamFile,
                  chunks,
                  mixContigs)
  }

  def createBamBins(regions: List[BedRecord],
                    bamFile: File,
                    chunks: Int,
                    mixContigs: Boolean = true): List[List[BedRecord]] = {
    val samReader = SamReaderFactory.makeDefault().open(bamFile)
    val dict = samReader.getFileHeader.getSequenceDictionary
    val index = samReader.indexing().getIndex
    val chunksEachRegion = for (region <- regions) yield {
      region -> index.getSpanOverlapping(dict.getSequenceIndex(region.chr),
                                         region.start,
                                         region.end)
    }
    val sizeEachRegion = chunksEachRegion.map {
      case (region, span) =>
        List(region) -> span.getChunks
          .map(c => c.getChunkEnd - c.getChunkStart)
          .sum
    }
    val totalSize = sizeEachRegion.map { case (_, length) => length }.sum
    val sizePerBin = totalSize / chunks
    if (sizePerBin > 0) {
      if (mixContigs) {
        createBamBinsRecurive(sizeEachRegion.filter {
          case (_, length) => length > 0
        }, sizePerBin, dict, index)
          .map { case (r, _) => r }
      } else {
        sizeEachRegion
          .groupBy(_._1.head.chr)
          .map(x =>
            createBamBinsRecurive(x._2, sizePerBin, dict, index)
              .map { case (r, _) => r })
          .reduce(_ ::: _)
      }
    } else Nil
  }

  @tailrec
  private def createBamBinsRecurive(
      regions: List[(List[BedRecord], Long)],
      sizePerBin: Long,
      dict: SAMSequenceDictionary,
      index: BAMIndex,
      minSize: Int = 200,
      iterations: Int = 1): List[(List[BedRecord], Long)] = {
    val (rebin, large, medium, small) = regions.foldLeft(
      (List[(List[BedRecord], Long)](),
       List[(List[BedRecord], Long)](),
       List[(List[BedRecord], Long)](),
       List[(List[BedRecord], Long)]())) {
      case ((r, l, m, s), (newList, newLength)) =>
        val large = newLength * 1.5 > sizePerBin
        val lessMinBp = newList.map(_.length).sum > minSize
        val smallMinSize = newLength * 0.5 < sizePerBin

        (large, lessMinBp, smallMinSize) match {
          case (true, true, _)  => ((newList, newLength) :: r, l, m, s)
          case (true, false, _) => (r, (newList, newLength) :: l, m, s)
          case (_, _, true)     => (r, l, m, (newList, newLength) :: s)
          case _                => (r, l, (newList, newLength) :: m, s)
        }
    }

    if (rebin.nonEmpty && iterations > 0) {
      val total = splitBins(rebin, sizePerBin, dict, index) ::: medium ::: combineBins(
        small,
        sizePerBin) ::: large
      createBamBinsRecurive(total, sizePerBin, dict, index, minSize, iterations - 1)
    } else {
      medium ::: combineBins(small, sizePerBin) ::: large ::: rebin
    }
  }

  private def combineBins(regions: List[(List[BedRecord], Long)],
                          sizePerBin: Long): List[(List[BedRecord], Long)] = {
    val (result, leftover) = regions
      .sortBy { case (_, l) => l }
      .foldLeft((List[(List[BedRecord], Long)](),
                 Option.empty[(List[BedRecord], Long)])) {
        case ((r, current), (newRegions, newLength)) =>
          current match {
            case Some((cr, cl)) =>
              if ((cl + newLength) > (sizePerBin * 1.5)) {
                ((cr, cl) :: r, Some((newRegions, newLength)))
              } else {
                (r, Some((cr ::: newRegions, cl + newLength)))
              }
            case _ => (r, Some((newRegions, newLength)))
          }
      }
    result ::: leftover.toList
  }

  private def splitBins(regions: List[(List[BedRecord], Long)],
                        sizePerBin: Long,
                        dict: SAMSequenceDictionary,
                        index: BAMIndex): List[(List[BedRecord], Long)] = {
    regions.flatMap {
      case (r, size) =>
        val chunks = {
          val x = size / sizePerBin
          if (x > 0) x else 1
        }
        val list = BedRecordList.fromList(r).combineOverlap
        val refSize = list.length
        val chunkSize = {
          val x = refSize / chunks
          x match {
            case _ if x > refSize => refSize
            case _ if x > 0       => x
            case _                => 1
          }
        }
        list.scatter(chunkSize.toInt).map { x =>
          val newSize = x
            .map(
              y =>
                index
                  .getSpanOverlapping(dict.getSequenceIndex(y.chr),
                                      y.start,
                                      y.end)
                  .getChunks
                  .map(z => z.getChunkEnd - z.getChunkStart)
                  .sum)
            .sum
          (x, newSize)
        }
    }
  }
}
