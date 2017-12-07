package nl.biopet.utils.ngs

import java.io.File

import htsjdk.variant.variantcontext.{Allele, Genotype, VariantContext}
import htsjdk.variant.vcf.{VCFFileReader, VCFHeader}
import nl.biopet.utils.conversions
import nl.biopet.utils.ngs.intervals.BedRecord

import java.util

import scala.collection.JavaConversions._

package object vcf {

  /**
    * Method will extend a allele till a new length
    * @param bases Allele
    * @param newSize New size of allele
    * @param fillWith Char to fill gap
    * @return
    */
  def fillAllele(bases: String, newSize: Int, fillWith: Char = '-'): String = {
    bases + Array.fill[Char](newSize - bases.length)(fillWith).mkString
  }

  /**
    * @deprecated Moved to nl.biopet.utils.conversions
    */
  def scalaListToJavaObjectArrayList(
      array: List[Any]): util.ArrayList[Object] = conversions.scalaListToJavaObjectArrayList(array)

  /**
    * Return true if header is a block-type GVCF file
    * @param header header of Vcf file
    * @return boolean
    */
  def isBlockGVcf(header: VCFHeader): Boolean = {
    header.getMetaDataLine("GVCFBlock") != null
  }

  /**
    * Get sample IDs from vcf File
    * @param vcf File object pointing to vcf
    * @return list of strings with sample IDs
    */
  def getSampleIds(vcf: File): List[String] = {
    val reader = new VCFFileReader(vcf, false)
    val samples = reader.getFileHeader.getSampleNamesInOrder.toList
    reader.close()
    samples
  }

  def getVcfIndexFile(vcfFile: File): File = {
    val name = vcfFile.getPath
    if (name.endsWith(".vcf")) new File(name + ".idx")
    else if (name.endsWith(".vcf.gz")) new File(name + ".tbi")
    else
      throw new IllegalArgumentException(
        s"File given is no vcf file: $vcfFile")
  }

  def vcfFileIsEmpty(file: File): Boolean = {
    val reader = new VCFFileReader(file, false)
    val hasNext = reader.iterator().hasNext
    reader.close()
    !hasNext
  }

  /** Give back the number of alleles that overlap */
  def alleleOverlap(g1: List[Allele], g2: List[Allele], start: Int = 0): Int = {
    if (g1.isEmpty) start
    else {
      val found = g2.contains(g1.head)
      val g2tail = if (found) {
        val index = g2.indexOf(g1.head)
        g2.drop(index + 1) ++ g2.take(index)
      } else g2

      alleleOverlap(g1.tail, g2tail, if (found) start + 1 else start)
    }
  }

  /** Give back the number of alleles that overlap */
  def alleleIndexOverlap(g1: List[Int], g2: List[Int], start: Int = 0): Int = {
    if (g1.isEmpty) start
    else {
      val found = g2.contains(g1.head)
      val g2tail = if (found) {
        val index = g2.indexOf(g1.head)
        g2.drop(index + 1) ++ g2.take(index)
      } else g2

      alleleIndexOverlap(g1.tail, g2tail, if (found) start + 1 else start)
    }
  }

  /**
    * Read all records of a single regions
    * @param inputFile input vcf file
    * @param region Region to fetch
    * @return Vcf records
    */
  def loadRegion(inputFile: File, region: BedRecord): Seq[VariantContext] = {
    val reader = new VCFFileReader(inputFile, true)
    val records = loadRegion(reader, region).toIndexedSeq
    reader.close()
    records
  }

  /**
    * Returns a iterator for records from region
    * @param reader reader to use
    * @param region Region to fetch
    * @return
    */
  def loadRegion(reader: VCFFileReader,
                 region: BedRecord): Iterator[VariantContext] = {
    val interval = region.toSamInterval
    reader.query(region.chr, interval.getStart, interval.getEnd)
  }

  /**
    * This method will return multiple region as a single iterator
    * @param inputFile input vcf file
    * @param regions regions to fetch, if regions does overlap
    * @return
    */
  def loadRegions(inputFile: File,
                  regions: Iterator[BedRecord]): Iterator[VariantContext] with AutoCloseable = {
    new Iterator[VariantContext] with AutoCloseable {
      private val reader = new VCFFileReader(inputFile, true)
      private val it = regions.flatMap(loadRegion(reader, _))

      def hasNext: Boolean = it.hasNext
      def next(): VariantContext = it.next()
      def close(): Unit = reader.close()
    }
  }

  implicit class BiopetVariantContext(record: VariantContext) {

    /**
      * Look up a list of Strings in the info fields
      * @param key Key to look up in the info fields
      * @param method methods to apply on list, default returns all values
      * @return
      */
    def getAttAsString(
        key: String,
        method: FieldMethod.Value = FieldMethod.All.asInstanceOf)
      : List[String] = {
      val value =
        if (record.hasAttribute(key))
          conversions.anyToStringList(Option(record.getAttribute(key)))
        else Nil
      method.apply(value)
    }

    /** Compare to an other VariantContext */
    def identicalVariantContext(other: VariantContext): Boolean = {
      record.getContig == other.getContig &&
      record.getStart == other.getStart &&
      record.getEnd == other.getEnd &&
      record.getAlleles == other.getAlleles &&
      record.getAttributes == other.getAttributes &&
        record.getGenotypesOrderedByName.zip(other.getGenotypesOrderedByName).forall(x => x._1.identicalGenotype(x._2))
    }

    /**
      * Return longest allele of VariantContext.
      *
      * @return length of the allele with the most nucleotides
      */
    def getLongestAlleleSize: Int = {
      record.getAlleles.map(_.getBases.length).max
    }
  }

  implicit class BiopetGenotype(genotype: Genotype) {

    /**
      * Look up a list of Strings in the genotype fields
      * @param key Key to look up in the genotype fields
      * @param method methods to apply on list, default returns all values
      * @return
      */
    def getAttAsString(
        key: String,
        method: FieldMethod.Value = FieldMethod.All.asInstanceOf)
      : List[String] = {
      val value =
        if (genotype.hasAnyAttribute(key))
          conversions.anyToStringList(genotype.getAnyAttribute(key))
        else Nil
      method.apply(value)
    }

    /** Compares to an other Genotype */
    def identicalGenotype(other: Genotype): Boolean = {
      genotype.getExtendedAttributes == other.getExtendedAttributes &&
      genotype.getAlleles == other.getAlleles &&
      genotype.getSampleName == other.getSampleName
    }

    /** Check whether genotype is of the form 0/. */
    def isCompoundNoCall: Boolean = {
      genotype.isCalled && genotype.getAlleles.exists(_.isNoCall) && genotype.getAlleles
        .exists(_.isReference)
    }

    /**
      * Check whether genotype has minimum genome Quality
      * @param minGQ minimum genome quality value
      * @return
      */
    def hasMinGenomeQuality(minGQ: Int): Boolean = {
      genotype.hasGQ && genotype.getGQ >= minGQ
    }
  }

}
