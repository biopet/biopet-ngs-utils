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

package nl.biopet.utils

package object ngs {

  /**
    * This method convert the sequence to a 2 bit encoding Integer
    * Size of the sequence of maximum 16 nucleotides
    * @param sequence sequence as string, max length is 16
    * @return
    */
  def sequenceTo2bitSingleInt(sequence: String): Int = {
    sequenceTo2bitSingleInt(sequence.getBytes)
  }

  val A: Byte = 'A'.toByte
  val T: Byte = 'T'.toByte
  val C: Byte = 'C'.toByte
  val G: Byte = 'G'.toByte
  val a: Byte = 'a'.toByte
  val t: Byte = 't'.toByte
  val c: Byte = 'c'.toByte
  val g: Byte = 'g'.toByte

  /**
    * This method convert the sequence to a 2 bit encoding Integer
    * Size of the sequence of maximum 16 nucleotides
    * If the length is smaller then 16 the rest is filled with A
    * @param sequence sequence as Array[Byte], max length is 16
    * @return
    */
  def sequenceTo2bitSingleInt(sequence: Array[Byte]): Int = {
    require(sequence.length <= 16, "Sequence is to long for a single Int")

    sequence.zipWithIndex.map {
      case (nuc, idx) =>
        (nuc match {
          case n if n == A || n == a => 0
          case n if n == T || n == t => 1
          case n if n == C || n == c => 2
          case n if n == G || n == g => 3
          case x =>
            throw new IllegalStateException(
              s"Only ATCG is allowed here, '${x.toChar}' is not")
        }) << (idx * 2)
    }.sum
  }

  /**
    * This convert a sequence string to a Array[Int]
    * @param sequence Sequence as string
    * @return Array of Int, each int represents 16 nucleotides
    */
  def sequenceTo2bitInt(sequence: String): Array[Int] = {
    sequenceTo2bitInt(sequence.getBytes)
  }

  /**
    * This convert a sequence string to a Array[Int]
    * @param sequence Sequence as string
    * @return Array of Int, each int represents 16 nucleotides
    */
  def sequenceTo2bitInt(sequence: Array[Byte]): Array[Int] = {
    sequence.grouped(16).map(sequenceTo2bitSingleInt).toArray
  }

  /**
    * Convert int back to sequence
    * @see sequenceTo2bitInt
    * @param sequence sequence as Array[Int]
    * @return
    */
  def int2bitToSequence(sequence: Array[Int]): Array[Byte] = {
    sequence.flatMap(x => int2bitToSequence(x))
  }

  /**
    * Convert int back to sequence
    * @see sequenceTo2bitSingleInt
    * @param sequence Sequnce as Int
    * @return
    */
  def int2bitToSequence(sequence: Int): Array[Byte] = {
    (0 to 16).map { i =>
      (sequence >> (i * 2)) % 4 match {
        case 0 => A
        case 1 => T
        case 2 => C
        case 3 => G
      }
    }.toArray
  }
}
