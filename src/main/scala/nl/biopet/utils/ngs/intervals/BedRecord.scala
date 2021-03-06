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

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.samtools.util.Interval
import nl.biopet.utils.ngs.fasta

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

/**
  * Created by pjvanthof on 20/08/15.
  */
case class BedRecord(
    chr: String,
    start: Int,
    end: Int,
    name: Option[String] = None,
    score: Option[Double] = None,
    strand: Option[Boolean] = None,
    thickStart: Option[Int] = None,
    thickEnd: Option[Int] = None,
    rgbColor: Option[(Int, Int, Int)] = None,
    blockCount: Option[Int] = None,
    blockSizes: IndexedSeq[Int] = IndexedSeq(),
    blockStarts: IndexedSeq[Int] = IndexedSeq(),
    protected[intervals] val _originals: List[BedRecord] = Nil) {

  def originals(nested: Boolean = true): List[BedRecord] = {
    _originals.isEmpty match {
      case (true)      => List(this)
      case _ if nested => _originals.flatMap(_.originals())
      case _           => _originals
    }
  }

  def overlapWith(record: BedRecord): Boolean = {
    chr == record.chr && start < record.end && record.start < end
  }

  def length: Int = end - start

  def scatter(binSize: Int): List[BedRecord] = {
    val binNumber = length / binSize
    if (binNumber <= 1) List(this)
    else {
      val size = length / binNumber
      val buffer = ListBuffer[BedRecord]()
      for (i <- 1 until binNumber)
        buffer += BedRecord(chr, start + ((i - 1) * size), start + (i * size))
      buffer += BedRecord(chr, start + ((binNumber - 1) * size), end)
      buffer.toList
    }
  }

  def getGc(referenceFile: IndexedFastaSequenceFile): Double = {
    fasta.getSequenceGc(referenceFile, chr, start, end)
  }

  lazy val exons: Option[IndexedSeq[BedRecord]] = {
    blockCount.filter(_ => blockSizes.nonEmpty && blockStarts.nonEmpty).map {
      c =>
        for (i <- 0 until c) yield {
          val exonNumber = strand match {
            case Some(false) => c - i
            case _           => i + 1
          }
          BedRecord(chr,
                    start + blockStarts(i),
                    start + blockStarts(i) + blockSizes(i),
                    Some(s"exon-$exonNumber"),
                    _originals = List(this))
        }
    }
  }

  lazy val introns: Option[IndexedSeq[BedRecord]] = {
    blockCount.filter(_ => blockSizes.nonEmpty && blockStarts.nonEmpty).map {
      c =>
        for (i <- 0 until (c - 1)) yield {
          val intronNumber = strand match {
            case Some(false) => c - i
            case _           => i + 1
          }
          BedRecord(chr,
                    start + blockStarts(i) + blockSizes(i),
                    start + blockStarts(i + 1),
                    Some(s"intron-$intronNumber"),
                    _originals = List(this))
        }
    }
  }

  lazy val utr5: Option[BedRecord] = (strand, thickStart, thickEnd) match {
    case (Some(true), Some(tStart), Some(tEnd))
        if tStart > start && tEnd < end =>
      Some(BedRecord(chr, start, tStart, name.map(_ + "_utr5")))
    case (Some(false), Some(tStart), Some(tEnd))
        if tStart > start && tEnd < end =>
      Some(BedRecord(chr, tEnd, end, name.map(_ + "_utr5")))
    case _ => None
  }

  lazy val utr3: Option[BedRecord] = (strand, thickStart, thickEnd) match {
    case (Some(false), Some(tStart), Some(tEnd))
        if tStart > start && tEnd < end =>
      Some(BedRecord(chr, start, tStart, name.map(_ + "_utr3")))
    case (Some(true), Some(tStart), Some(tEnd))
        if tStart > start && tEnd < end =>
      Some(BedRecord(chr, tEnd, end, name.map(_ + "_utr3")))
    case _ => None
  }

  override def toString: String = {
    def arrayToOption[T](array: IndexedSeq[T]): Option[IndexedSeq[T]] = {
      if (array.isEmpty) None
      else Some(array)
    }
    List(
      Some(chr),
      Some(start),
      Some(end),
      name,
      score,
      strand.map(if (_) "+" else "-"),
      thickStart,
      thickEnd,
      rgbColor.map { case (r, g, b) => s"$r,$g,$b" },
      blockCount,
      arrayToOption(blockSizes).map(_.mkString(",")),
      arrayToOption(blockStarts).map(_.mkString(","))
    ).takeWhile(_.isDefined)
      .flatten
      .mkString("\t")
  }

  def validate: BedRecord = {
    require(start < end, "Start is greater then end")
    (thickStart, thickEnd) match {
      case (Some(s), Some(e)) =>
        require(s <= e, "Thick start is greater then end")
      case _ =>
    }
    blockCount match {
      case Some(count) =>
        require(count == blockSizes.length,
                "Number of sizes is not the same as blockCount")
        require(count == blockStarts.length,
                "Number of starts is not the same as blockCount")
      case _ =>
    }
    this
  }

  def toSamInterval: Interval = (name, strand) match {
    case (Some(name), Some(strand)) =>
      new Interval(chr, start + 1, end, !strand, name)
    case (Some(name), _) => new Interval(chr, start + 1, end, false, name)
    case _               => new Interval(chr, start + 1, end)
  }
}

object BedRecord {
  def fromLine(line: String): BedRecord = {
    val values = line.split("\t")
    require(values.length >= 3, "Not enough columns count for a bed file")
    BedRecord(
      values(0),
      values(1).toInt,
      values(2).toInt,
      values.lift(3),
      values.lift(4).map(_.toDouble),
      values.lift(5).map {
        case "-" => false
        case "+" => true
        case _ =>
          throw new IllegalStateException(
            "Strand (column 6) must be '+' or '-'")
      },
      values.lift(6).map(_.toInt),
      values.lift(7) map (_.toInt),
      values
        .lift(8)
        .map(_.split(",", 3).map(_.toInt))
        .map(
          x =>
            (x.headOption.getOrElse(0),
             x.lift(1).getOrElse(0),
             x.lift(2).getOrElse(0))),
      values.lift(9).map(_.toInt),
      values
        .lift(10)
        .map(_.split(",").map(_.toInt).toIndexedSeq)
        .getOrElse(IndexedSeq()),
      values
        .lift(11)
        .map(_.split(",").map(_.toInt).toIndexedSeq)
        .getOrElse(IndexedSeq())
    )
  }
}
