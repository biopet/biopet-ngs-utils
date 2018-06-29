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

package nl.biopet.utils.ngs.intervals

import java.io.{File, PrintWriter}

import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.samtools.util.Interval
import nl.biopet.utils.ngs.fasta

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.NonFatal

/**
  * Created by pjvan_thof on 8/20/15.
  */
case class BedRecordList(chrRecords: Map[String, List[BedRecord]],
                         header: List[String] = Nil) {
  def allRecords: Iterable[BedRecord] =
    chrRecords.values.flatten

  def toSamIntervals: Iterable[Interval] =
    allRecords.map(_.toSamInterval)

  lazy val sorted: BedRecordList = {
    val sorted = new BedRecordList(chrRecords.map {
      case (key, values) => key -> values.sortWith((a, b) => a.start < b.start)
    })
    if (sorted.chrRecords.forall {
          case (key, values) => values == chrRecords(key)
        }) this
    else sorted
  }

  lazy val isSorted: Boolean = sorted.hashCode() == this
    .hashCode() || sorted.chrRecords.forall {
    case (key, values) => values == chrRecords(key)
  }

  def overlapWith(record: BedRecord): List[BedRecord] =
    sorted.chrRecords
      .getOrElse(record.chr, Nil)
      .dropWhile(_.end <= record.start)
      .takeWhile(_.start < record.end)

  def length: Long = allRecords.foldLeft(0L)((a, b) => a + b.length)

  def squishBed(strandSensitive: Boolean = true,
                nameSensitive: Boolean = true): BedRecordList =
    BedRecordList.fromList {
      (for ((_, records) <- sorted.chrRecords; record <- records) yield {
        val overlaps = overlapWith(record)
          .filterNot(_ == record)
          .filterNot(strandSensitive && _.strand != record.strand)
          .filterNot(nameSensitive && _.name == record.name)
        if (overlaps.isEmpty) {
          List(record)
        } else {
          overlaps
            .foldLeft(List(record))((result, overlap) => {
              (for (r <- result) yield {
                if (r.overlapWith(overlap)) {
                  (overlap.start <= r.start, overlap.end >= r.end) match {
                    case (true, true) =>
                      Nil
                    case (true, false) =>
                      List(r.copy(start = overlap.end, _originals = List(r)))
                    case (false, true) =>
                      List(r.copy(end = overlap.start, _originals = List(r)))
                    case (false, false) =>
                      List(r.copy(end = overlap.start, _originals = List(r)),
                           r.copy(start = overlap.end, _originals = List(r)))
                  }
                } else List(r)
              }).flatten
            })
        }
      }).flatten
    }

  def combineOverlap: BedRecordList = {
    new BedRecordList(
      for ((chr, records) <- sorted.chrRecords)
        yield
          chr -> {
            def combineOverlap(records: List[BedRecord],
                               newRecords: ListBuffer[BedRecord] = ListBuffer())
              : List[BedRecord] = {
              records.headOption match {
                case Some(record) =>
                  val chr = record.chr
                  val start = record.start
                  val overlapRecords =
                    records.takeWhile(_.start <= record.end)
                  val end = overlapRecords.map(_.end).max

                  newRecords += BedRecord(chr,
                                          start,
                                          end,
                                          _originals = overlapRecords)
                  combineOverlap(records.drop(overlapRecords.length),
                                 newRecords)
                case _ => newRecords.toList
              }
            }
            combineOverlap(records)
          })
  }

  def scatter(binSize: Int,
              combineContigs: Boolean = true,
              maxContigsInSingleJob: Option[Int] = None,
              sequenceDict: Option[SAMSequenceDictionary] = None)
    : List[List[BedRecord]] = {
    val (list, leftover) = allRecords
      .flatMap(_.scatter(binSize))
      .toList
      .sortWith(sequenceDict match {
        case Some(order) => // Sort by chromosome, start position if sequenceDict is given
          (l, r) =>
            order.getSequenceIndex(l.chr) < order.getSequenceIndex(r.chr) ||
            (order.getSequenceIndex(l.chr) == order.getSequenceIndex(r.chr) &&
            l.start < r.start)
        case _ => // Otherwise sort by length
          (l, r) =>
            l.length < r.length
      })
      .reverse
      .foldLeft((List[List[BedRecord]](), List[BedRecord]())) {
        case ((finalList, buffer), record) =>
          val bufferSize = buffer.map(_.length).sum
          buffer.headOption match {
            case Some(r) if !combineContigs && r.chr != record.chr =>
              (buffer :: finalList, List(record))
            case Some(_)
                if bufferSize < (binSize / 2) &&
                  buffer.size < maxContigsInSingleJob.getOrElse(Int.MaxValue) =>
              (finalList, record :: buffer)
            case _ => (finalList, record :: buffer)
          }
      }
    leftover :: list
  }

  def validateContigs(reference: File): BedRecordList = {
    this.validateContigs(fasta.getCachedDict(reference))
  }

  def validateContigs(dict: SAMSequenceDictionary): BedRecordList = {
    val notExisting =
      chrRecords.keys.filter(x => Option(dict.getSequence(x)).isEmpty).toList
    require(
      notExisting.isEmpty,
      s"Contigs found in bed records but are not existing in reference: ${notExisting
        .mkString(",")}")
    this
  }

  def writeToFile(file: File): Unit = {
    val writer = new PrintWriter(file)
    header.foreach(writer.println)
    allRecords.foreach(writer.println)
    writer.close()
  }

  def getGc(referenceFile: IndexedFastaSequenceFile): Double = {
    allRecords.map(r => r.getGc(referenceFile) * r.length).sum / length
  }

  /** This return the fraction of the regions comparing to a length */
  def fractionOf(length: Long): Double = this.length.toDouble / length.toDouble

  /** This return the fraction of the regions comparing to a reference */
  def fractionOfReference(dict: SAMSequenceDictionary): Double =
    fractionOf(dict.getReferenceLength)

  /** This return the fraction of the regions comparing to a reference */
  def fractionOfReference(file: File): Double =
    fractionOfReference(fasta.getCachedDict(file))
}

object BedRecordList {
  def fromListWithHeader(records: Traversable[BedRecord],
                         header: List[String]): BedRecordList =
    fromListWithHeader(records.toIterator, header)

  def fromListWithHeader(records: TraversableOnce[BedRecord],
                         header: List[String]): BedRecordList = {
    val map = mutable.Map[String, ListBuffer[BedRecord]]()
    for (record <- records) {
      if (!map.contains(record.chr)) map += record.chr -> ListBuffer()
      map(record.chr) += record
    }
    new BedRecordList(map.toMap.map {
      case (key, values) => key -> values.toList
    }, header)
  }

  def fromList(records: Traversable[BedRecord]): BedRecordList =
    fromListWithHeader(records.toIterator, Nil)

  def fromList(records: TraversableOnce[BedRecord]): BedRecordList =
    fromListWithHeader(records, Nil)

  /**
    * This creates a [[BedRecordList]] based on multiple files. This method combines overlapping regions
    *
    * @param bedFiles Input bed files
    * @return
    */
  def fromFilesCombine(bedFiles: File*): BedRecordList = {
    fromFiles(bedFiles, combine = true)
  }

  /**
    * This creates a [[BedRecordList]] based on multiple files
    *
    * @param bedFiles Input bed files
    * @param combine When true overlaping regions are merged
    * @return
    */
  def fromFiles(bedFiles: Seq[File],
                combine: Boolean = false): BedRecordList = {
    val list = bedFiles.foldLeft(empty)((a, b) =>
      fromList(fromFile(b).allRecords ++ a.allRecords))
    if (combine) list.combineOverlap
    else list
  }

  /** This created a empty [[BedRecordList]] */
  def empty: BedRecordList = fromList(Nil)

  def fromFile(bedFile: File): BedRecordList = {
    val reader = Source.fromFile(bedFile)
    val all = reader.getLines().toList
    val header =
      all.takeWhile(x => x.startsWith("browser") || x.startsWith("track"))
    val headerLength = header.length
    try {
      fromListWithHeader(
        all.zipWithIndex.drop(headerLength).map {
          case (line, idx) => {
            try {
              BedRecord.fromLine(line).validate
            } catch {
              case NonFatal(e) =>
                throw new IllegalStateException(
                  s"Parsing line number ${idx + 1} failed on file: ${bedFile.getAbsolutePath}",
                  e)
            }
          }
        },
        header
      )
    } finally {
      reader.close()
    }
  }

  def fromReference(file: File): BedRecordList =
    fromDict(fasta.getCachedDict(file))

  def fromDict(dict: SAMSequenceDictionary): BedRecordList = {
    fromList(for (contig <- dict.getSequences) yield {
      BedRecord(contig.getSequenceName, 0, contig.getSequenceLength)
    })
  }
}
