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
    readContigMap(file).flatMap(x => x._2.map(y => y -> x._1))
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
            .filter(x => x._1 == name || x._2.contains(name))
            .flatMap(x => x._2 + x._1)
            .filter(_ != name)
            .toSet
        else
          map
            .filter(x =>
              x._1.toLowerCase == name.toLowerCase || x._2.exists(
                name.toLowerCase == _.toLowerCase))
            .flatMap(x => x._2 + x._1)
            .filter(_.toLowerCase != name.toLowerCase)
            .toSet
      name -> set
    }).toMap
  }
}
