package nl.biopet.utils.ngs.bam

import java.io.File

import htsjdk.samtools.{BAMIndex, SAMSequenceDictionary, SamReaderFactory}
import nl.biopet.utils.ngs.intervals.{BedRecord, BedRecordList}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

object IndexScattering {
  def creatBamBins(bamFile: File, chunks: Int): List[List[BedRecord]] = {
    val samReader = SamReaderFactory.makeDefault().open(bamFile)
    val dict = samReader.getFileHeader.getSequenceDictionary
    samReader.close()
    creatBamBins(BedRecordList.fromDict(dict).allRecords.toList, bamFile, chunks)
  }

  def creatBamBins(regions: List[BedRecord], bamFile: File, chunks: Int): List[List[BedRecord]] = {
    val samReader = SamReaderFactory.makeDefault().open(bamFile)
    val dict = samReader.getFileHeader.getSequenceDictionary
    val index = samReader.indexing().getIndex
    val chunksEachRegion = for (region <- regions) yield {
      region -> index.getSpanOverlapping(dict.getSequenceIndex(region.chr),
        region.start,
        region.end)
    }
    val sizeEachRegion = chunksEachRegion.map(s =>
      List(s._1) -> s._2.getChunks
        .map(c => c.getChunkEnd - c.getChunkStart)
        .sum)
    val totalSize = sizeEachRegion.map(_._2).sum
    val sizePerBin = totalSize / chunks
    createBamBins(sizeEachRegion.filter(_._2 > 0),
      sizePerBin,
      dict,
      index).map(_._1)

  }

  @tailrec
  private def createBamBins(
                             regions: List[(List[BedRecord], Long)],
                             sizePerBin: Long,
                             dict: SAMSequenceDictionary,
                             index: BAMIndex,
                             minSize: Int = 200, iterations: Int = 1): List[(List[BedRecord], Long)] = {
    val largeContigs = regions.filter(_._2 * 1.5 > sizePerBin)
    val rebin = largeContigs.filter(_._1.map(_.length).sum > minSize)
    val mediumContigs = regions.filter(c =>
      c._2 * 1.5 <= sizePerBin && c._2 * 0.5 >= sizePerBin) ::: largeContigs
      .filter(_._1.map(_.length).sum <= minSize)
    val smallContigs = regions.filter(_._2 * 0.5 < sizePerBin)

    if (rebin.nonEmpty && iterations > 0) {
      val total = splitBins(rebin, sizePerBin, dict, index) ::: mediumContigs ::: combineBins(
        smallContigs,
        sizePerBin)
      createBamBins(total, sizePerBin, dict, index, minSize, iterations - 1)
    } else mediumContigs ::: combineBins(smallContigs, sizePerBin)
  }

  private def combineBins(regions: List[(List[BedRecord], Long)],
                          sizePerBin: Long): List[(List[BedRecord], Long)] = {
    val result = regions
      .sortBy(_._2)
      .foldLeft((List[(List[BedRecord], Long)](),
        Option.empty[(List[BedRecord], Long)])) {
        case ((r, current), newRecord) =>
          current match {
            case Some(c) =>
              if ((c._2 + newRecord._2) > (sizePerBin * 1.5)) {
                (c :: r, Some(newRecord))
              } else {
                (r, Some((c._1 ::: newRecord._1, c._2 + newRecord._2)))
              }
            case _ => (r, Some(newRecord))
          }
      }
    result._1 ::: result._2.toList
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
          if (x > refSize) refSize else if (x > 0) x else 1
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
