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

package nl.biopet.utils.ngs

import java.io.File

import htsjdk.samtools.{
  SAMReadGroupRecord,
  SAMSequenceDictionary,
  SamReader,
  SamReaderFactory
}

import scala.collection.JavaConversions._
import scala.collection.parallel.immutable

package object bam {

  /**
    * This method will convert a list of bam files to a Map[<sampleName>, <bamFile>]
    *
    * Each sample may only be once in the list
    *
    * @throws IllegalArgumentException
    * @param bamFiles input bam files
    * @return Map of sample bam files
    */
  def sampleBamMap(bamFiles: List[File]): Map[String, File] = {
    val readers = sampleBamReaderMap(bamFiles)
    readers.foreach(_._2._1.close())
    readers.map(x => x._1 -> x._2._2)
  }

  def sampleBamReaderMap(
      bamFiles: List[File]): Map[String, (SamReader, File)] = {
    val temp = bamFiles.map { file =>
      val inputSam = SamReaderFactory.makeDefault.open(file)
      val samples =
        inputSam.getFileHeader.getReadGroups.map(_.getSample).distinct
      if (samples.size == 1) samples.head -> (inputSam, file)
      else if (samples.size > 1)
        throw new IllegalArgumentException(
          "Bam contains multiple sample IDs: " + file)
      else
        throw new IllegalArgumentException(
          "Bam does not contain sample ID or have no readgroups defined: " + file)
    }
    if (temp.map(_._1).distinct.size != temp.size)
      throw new IllegalArgumentException("Samples has been found twice")
    temp.toMap
  }

  /**
    * This method will return all readgroups for each sample
    *
    * @throws IllegalArgumentException
    * @param bamFiles input bam files
    * @return Map of sample readgroups
    */
  def sampleReadGroups(
      bamFiles: List[File]): Map[String, List[SAMReadGroupRecord]] = {
    val sampleBamFiles = sampleBamMap(bamFiles)
    sampleBamFiles.map {
      case (sampleName, bamFile) =>
        val inputSam = SamReaderFactory.makeDefault.open(bamFile)
        val header = inputSam.getFileHeader
        inputSam.close()
        sampleName -> header.getReadGroups.toList
    }
  }

  def sampleReadGroups(readers: Map[String, (SamReader, File)])
    : Map[String, List[SAMReadGroupRecord]] = {
    readers.map {
      case (sampleName, (reader, bamFile)) =>
        sampleName -> reader.getFileHeader.getReadGroups.toList
    }
  }

  /**
    * Estimate the insertsize of fragments within the given contig.
    * Uses the properly paired reads according to flags set by the aligner
    *
    * @deprecated Please use [[nl.biopet.utils.ngs.bam.InsertsizeEstimate.regionsInsertSize]]
    */
  def contigInsertSize(inputBam: File,
                       contig: String,
                       start: Int,
                       end: Int,
                       samplingSize: Int = 10000,
                       binSize: Int = 1000000): Option[Int] =
    InsertsizeEstimate.regionsInsertSize(inputBam,
                                         contig,
                                         start,
                                         end,
                                         samplingSize,
                                         binSize)

  /**
    * Estimate the insertsize for one single bamfile and return the insertsize
    *
    * @param bamFile bamfile to estimate average insertsize from
    * @return
    */
  def sampleBamInsertSize(bamFile: File,
                          samplingSize: Int = 10000,
                          binSize: Int = 1000000): Int = {
    val inputSam: SamReader = SamReaderFactory.makeDefault.open(bamFile)
    val bamInsertSizes =
      inputSam.getFileHeader.getSequenceDictionary.getSequences.par
        .map({ contig =>
          bam.contigInsertSize(bamFile,
                               contig.getSequenceName,
                               1,
                               contig.getSequenceLength,
                               samplingSize,
                               binSize)
        })
        .toList
    val counts = bamInsertSizes.flatten

    // avoid division by zero
    if (counts.nonEmpty) counts.sum / counts.size
    else 0
  }

  /**
    * Estimate the insertsize for each bam file and return Map[<sampleBamFile>, <insertSize>]
    *
    * @param bamFiles input bam files
    * @return
    */
  def sampleBamsInsertSize(
      bamFiles: List[File],
      samplingSize: Int = 10000,
      binSize: Int = 1000000): immutable.ParMap[File, Int] =
    bamFiles.par.map { bamFile =>
      bamFile -> sampleBamInsertSize(bamFile, samplingSize, binSize)
    }.toMap

  /** This class will add functionality to [[SAMSequenceDictionary]] */
  implicit class BiopetSamDict(samDicts: SAMSequenceDictionary) {

    /**
      * This method will check if all contig and sizes are the same without looking at the order of the contigs
      *
      * @throws AssertionError
      * @param that Dict to compare to
      * @param ignoreOrder When true the order of the contig does not matter
      */
    def assertSameDictionary(that: SAMSequenceDictionary,
                             ignoreOrder: Boolean): Unit = {
      if (ignoreOrder) {
        assert(samDicts.getReferenceLength == that.getReferenceLength)
        val thisContigNames =
          samDicts.getSequences
            .map(x => (x.getSequenceName, x.getSequenceLength))
            .sorted
            .toSet
        assert(
          thisContigNames == that.getSequences
            .map(x => (x.getSequenceName, x.getSequenceLength))
            .sorted
            .toSet)
      } else samDicts.assertSameDictionary(that)
    }
  }

}
