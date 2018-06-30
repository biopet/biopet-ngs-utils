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
        case Some(true)  => "+"
        case Some(false) => "-"
        case None        => "."
      },
      frame.getOrElse("."),
      attributes.map { case (k, v) => k + s""" "$v"""" }.mkString("; ")
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
        case Some(true)  => "+"
        case Some(false) => "-"
        case None        => "."
      },
      frame.getOrElse("."),
      attributes.map { case (k, v) => k + s"=$v" }.mkString(";")
    ).mkString("\t")

  def minPosition: Int = if (start < end) start else end
  def maxPosition: Int = if (start > end) start else end
}

object Feature {
  def fromLine(line: String): Feature = {
    val values = line.split("\t")

    require(values.size == 9 || values.size == 8,
            s"A Gtf line should have 8 or 9 columns, gtf line: '$line'")

    Feature(
      values(0),
      values(1),
      values(2),
      values(3).toInt,
      values(4).toInt,
      parseScore(values(5)),
      parseStrand(values(6)),
      parseFrame(values(7)),
      parseAttributes(values.lift(8))
    )
  }

  def parseStrand(value: String): Option[Boolean] = value match {
    case "+" => Some(true)
    case "-" => Some(false)
    case "." => None
    case _ =>
      throw new IllegalArgumentException(
        s"strand only allows '+' or '-', not $value")
  }

  def parseFrame(value: String): Option[Char] = value match {
    case "."                        => None
    case s: String if s.length == 1 => Some(s.head)
    case s =>
      throw new IllegalArgumentException(s"'$s' can not be parsed as frame")
  }

  def parseAttributes(value: Option[String]): Map[String, String] =
    value
      .map(_.split(";"))
      .getOrElse(Array())
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(parseAttribute)
      .toMap

  def parseScore(value: String): Option[Double] =
    try {
      value match {
        case "."       => None
        case s: String => Some(s.toDouble)
      }
    } catch {
      case e: NumberFormatException =>
        throw new IllegalStateException("Score in a gtf line must be number", e)
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
