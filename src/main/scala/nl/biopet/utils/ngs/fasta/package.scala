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

import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.reference.{FastaSequenceFile, IndexedFastaSequenceFile}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

package object fasta {

  /**
    * This method will get a dict from the fasta file. This will not use the cache
    *
    * @param fastaFile Fasta file
    * @return sequence dict
    */
  def getDictFromFasta(fastaFile: File): SAMSequenceDictionary = {
    val referenceFile = new FastaSequenceFile(fastaFile, true)
    val dict = referenceFile.getSequenceDictionary
    referenceFile.close()
    dictCache += fastaFile -> dict
    dict
  }

  private val dictCache: mutable.Map[File, SAMSequenceDictionary] =
    mutable.Map()

  /** This will clear the dict cache */
  def clearCache(): Unit = {
    dictCache.clear()
  }

  /**
    * This method will get a dict from the fasta file. If it's already in the cache file will not opened again.
    *
    * @param fastaFile Fasta file
    * @return sequence dict
    */
  def getCachedDict(fastaFile: File): SAMSequenceDictionary = {
    if (!dictCache.contains(fastaFile)) getDictFromFasta(fastaFile)
    else dictCache(fastaFile)
  }

  /** This method returns the fraction of GC for a given region */
  def getSequenceGc(fastaFile: File,
                    contig: String,
                    start: Long,
                    end: Long): Double = {
    getSequenceGc(new IndexedFastaSequenceFile(fastaFile), contig, start, end)
  }

  /** This method returns the fraction of GC for a given region */
  def getSequenceGc(referenceFile: IndexedFastaSequenceFile,
                    contig: String,
                    start: Long,
                    end: Long): Double = {
    require(referenceFile.isIndexed)
    val sequence = referenceFile.getSubsequenceAt(contig, start, end)
    val gcCount =
      sequence.getBaseString.toLowerCase.count(c => c == 'c' || c == 'g')
    val atCount =
      sequence.getBaseString.toLowerCase.count(c => c == 'a' || c == 't')
    val gc = gcCount.toDouble / (gcCount + atCount)
    gc
  }

  def readContigMap(file: File): Map[String, Set[String]] = {
    val reader = Source.fromFile(file)
    val map = reader
      .getLines()
      .filter(!_.startsWith("#"))
      .map { line =>
        val columns = line.split("\t")
        val refContig = columns(0)
        val alternativeNames =
          columns.lift(1).map(_.split(";").toSet).getOrElse(Set())
        refContig -> alternativeNames
      }
      .toMap
    reader.close()
    map
  }

  def readContigMapReverse(file: File): Map[String, String] = {
    readContigMap(file).flatMap { case (k, v) => v.map(y => y -> k) }
  }

  def rebuildContigMap(
      contigMap: File,
      referenceFasta: File,
      caseSentive: Boolean = false): Map[String, Set[String]] = {
    rebuildContigMap(contigMap, getCachedDict(referenceFasta), caseSentive)
  }

  def rebuildContigMap(contigMap: File,
                       dict: SAMSequenceDictionary,
                       caseSentive: Boolean): Map[String, Set[String]] = {
    val map = readContigMap(contigMap)
    (for (contig <- dict.getSequences) yield {

      val name = contig.getSequenceName
      val set =
        if (caseSentive)
          map
            .filter { case (k, v) => k == name || v.contains(name) }
            .flatMap { case (k, v) => v + k }
            .filter(_ != name)
            .toSet
        else
          map
            .filter {
              case (k, v) =>
                k.toLowerCase == name.toLowerCase || v.exists(
                  name.toLowerCase == _.toLowerCase)
            }
            .flatMap { case (k, v) => v + k }
            .filter(_.toLowerCase != name.toLowerCase)
            .toSet
      name -> set
    }).toMap
  }
}
