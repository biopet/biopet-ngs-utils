package nl.biopet.utils.ngs.annotation

import scala.language.postfixOps
import scala.util.matching.Regex

/**
  * This class can store a gtf or gff line.
  *
  * Created by pjvanthof on 14/05/2017.
  */
case class Feature(contig: String,
                   source: String,
                   feature: String,
                   start: Int,
                   end: Int,
                   score: Option[Double],
                   strand: Option[Boolean],
                   frame: Option[Char],
                   attributes: Map[String, String]) {

  def asGtfLine: String =
    List(
      contig,
      source,
      feature,
      start,
      end,
      score.getOrElse("."),
      strand match {
        case Some(true) => "+"
        case Some(false) => "-"
        case None => "."
      },
      frame.getOrElse("."),
      attributes.map(x => x._1 + s""" "${x._2}"""").mkString("; ")
    ).mkString("\t")

  def asGff3Line: String =
    List(
      contig,
      source,
      feature,
      start,
      end,
      score.getOrElse("."),
      strand match {
        case Some(true) => "+"
        case Some(false) => "-"
        case None => "."
      },
      frame.getOrElse("."),
      attributes.map(x => x._1 + s"=${x._2}").mkString(";")
    ).mkString("\t")

  def minPosition: Int = if (start < end) start else end
  def maxPosition: Int = if (start > end) start else end
}

object Feature {
  def fromLine(line: String): Feature = {
    val values = line.split("\t")

    require(values.size == 9 || values.size == 8,
            s"A Gtf line should have 8 or 9 columns, gtf line: '$line'")

    val strand = values(6) match {
      case "+" => Some(true)
      case "-" => Some(false)
      case "." => None
      case _ =>
        throw new IllegalArgumentException(
          s"strand only allows '+' or '-', not ${values(6)}, gtf line: $line")
    }

    val attributes = values
      .lift(8)
      .map(_.split(";"))
      .getOrElse(Array())
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(parseAttribute)
      .toMap

    val score = try {
      values(5) match {
        case "." => None
        case s: String => Some(s.toDouble)
      }
    } catch {
      case e: NumberFormatException =>
        throw new IllegalStateException("Score in a gtf line must be number",
                                        e)
    }

    val frame = values(7) match {
      case "." => None
      case s: String if s.length == 1 => Some(s.head)
      case s =>
        throw new IllegalArgumentException(
          s"'$s' can not be parsed as frame, gtf line: $line")
    }

    Feature(values(0),
            values(1),
            values(2),
            values(3).toInt,
            values(4).toInt,
            score,
            strand,
            frame,
            attributes)
  }

  def parseAttribute(att: String): (String, String) = {
    att match {
      case attributesGtfRegex(key, value) => key -> value
      case attributesGffRegex(key, value) => key -> value
      case x =>
        val values = x.trim.split(" ")
        require(values.length == 2, s"Attribute '$x' it not correct formatted")
        values(0) -> values(1).stripSuffix("\'").stripPrefix("\"")

    }
  }

  lazy val attributesGtfRegex: Regex = """\s*(\S*) "?(.*)"$""".r
  lazy val attributesGffRegex: Regex = """\s*(\S*)=(.*)$""".r
}
