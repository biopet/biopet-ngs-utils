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

package nl.biopet.utils.ngs.vcf

import htsjdk.variant.vcf.VCFHeader

case class VcfField(key: String, method: FieldMethod.Value) {
  def newInfoCount(header: VCFHeader): InfoFieldCounts = {
    new InfoFieldCounts(header.getInfoHeaderLine(key), method)
  }

  def newGenotypeCount(header: VCFHeader): GenotypeFieldCounts = {
    new GenotypeFieldCounts(header, header.getFormatHeaderLine(key), method)
  }

  override def toString = s"${key}_$method"
}

object VcfField {
  def fromArg(arg: String): VcfField = {
    val values = arg.split(":")
    require(
      values.size == 2,
      s"A field should be formatted like: <tag>:<method>. Possible methods: ${FieldMethod.values
        .mkString(", ")}")
    val key: String = values.head
    val method: FieldMethod.Value = try {
      FieldMethod.withName(values(1))
    } catch {
      case e: NoSuchElementException =>
        throw new IllegalArgumentException(
          s"Method '${values(1)}' does not exis. Possible methods: ${FieldMethod.values
            .mkString(", ")}t",
          e)
    }
    VcfField(key, method)
  }
}
