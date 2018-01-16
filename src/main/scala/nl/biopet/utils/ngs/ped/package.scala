package nl.biopet.utils.ngs

package object ped {

  object Gender extends Enumeration {
    val Male = Value(1)
    val Female = Value(2)
    val Unknown = Value(0)
  }

  object Phenotype extends Enumeration {
    val Unaffected = Value(1)
    val Affected = Value(2)
    val Missing = Value(0)
  }
}
