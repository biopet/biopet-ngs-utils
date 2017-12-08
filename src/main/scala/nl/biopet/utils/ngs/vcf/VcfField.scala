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
