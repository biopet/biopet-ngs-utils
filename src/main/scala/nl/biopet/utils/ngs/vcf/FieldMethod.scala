package nl.biopet.utils.ngs.vcf


import scala.language.implicitConversions

object FieldMethod extends Enumeration {
  protected case class Val(doubleList: List[Double] => List[Double]) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  implicit def valToValue(x: Val): Value = x.asInstanceOf[Value]

  val Min = Val(_.min :: Nil)
  val Max   = Val(_.max :: Nil)
  val Unique   = Val(_.distinct)
  val All    = Val(_)
}

