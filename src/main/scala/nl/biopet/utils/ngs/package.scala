package nl.biopet.utils

package object ngs {
  /**
    * This method convert the sequence to a 2 bit encoding Integer
    * Size of the sequence of maximum 16 nucleotides
    * @param sequence sequence as string, max length is 16
    * @return
    */
  def sequenceTo2bitInt(sequence: String): Int = {
    sequenceTo2bitInt(sequence.getBytes)
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
  def sequenceTo2bitInt(sequence: Array[Byte]): Int = {
    require(sequence.length <= 16, "Sequence is to long for a single Int")

    sequence.zipWithIndex.map { case (nuc, idx) =>
      (nuc match {
        case n if n == A || n == a => 0
        case n if n == T || n == t => 1
        case n if n == C || n == c => 2
        case n if n == G || n == g => 3
        case x => throw new IllegalStateException(s"Only ATCG is allowed here, '${x.toChar}' is not")
      }) << (idx * 2)
    }.sum
  }

  /**
    * Convert int back to sequence
    * @see
    * @param sequence
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
