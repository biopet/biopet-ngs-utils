package nl.biopet.utils.ngs.bam

import java.io.File

import htsjdk.samtools.{BAMIndex, SAMSequenceDictionary, SamReaderFactory}
import nl.biopet.utils.ngs.intervals.{BedRecord, BedRecordList}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

object IndexScattering {
  def createBamBins(bamFile: File, chunks: Int): List[List[BedRecord]] = {
    val samReader = SamReaderFactory.makeDefault().open(bamFile)
    val dict = samReader.getFileHeader.getSequenceDictionary
    samReader.close()
    createBamBins(BedRecordList.fromDict(dict).allRecords.toList,
                 bamFile,
                 chunks)
  }

  def createBamBins(regions: List[BedRecord],
                    bamFile: File,
                    chunks: Int): List[List[BedRecord]] = {
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
    createBamBins(sizeEachRegion.filter { case (_, length) => length > 0 },
                  sizePerBin,
                  dict,
                  index)
      .map { case (r, _) => r }

  }

  @tailrec
  private def createBamBins(
      regions: List[(List[BedRecord], Long)],
      sizePerBin: Long,
      dict: SAMSequenceDictionary,
      index: BAMIndex,
      minSize: Int = 200,
      iterations: Int = 1): List[(List[BedRecord], Long)] = {
    val largeContigs = regions.filter { case (_, l) => l * 1.5 > sizePerBin }
    val rebin = largeContigs.filter {
      case (cr, _) => cr.map(_.length).sum > minSize
    }
    val mediumContigs = regions.filter {
      case (_, l) =>
        l * 1.5 <= sizePerBin && l * 0.5 >= sizePerBin
    } ::: largeContigs
      .filter { case (cr, _)                        => cr.map(_.length).sum <= minSize }
    val smallContigs = regions.filter { case (_, l) => l * 0.5 < sizePerBin }

    if (rebin.nonEmpty && iterations > 0) {
      val total = splitBins(rebin, sizePerBin, dict, index) ::: mediumContigs ::: combineBins(
        smallContigs,
        sizePerBin)
      createBamBins(total, sizePerBin, dict, index, minSize, iterations - 1)
    } else mediumContigs ::: combineBins(smallContigs, sizePerBin)
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
