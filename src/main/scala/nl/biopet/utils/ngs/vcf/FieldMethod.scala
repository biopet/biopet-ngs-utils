package nl.biopet.utils.ngs.vcf

import scala.language.implicitConversions

object FieldMethod extends Enumeration {
  protected case class Val(apply: List[String] => List[String])
      extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  implicit def valToValue(x: Val): Value = x.asInstanceOf[Value]

  val Min = Val(
    x => if (x.isEmpty) Nil else x.map(_.toDouble).min.toString :: Nil)
  val Max = Val(
    x => if (x.isEmpty) Nil else x.map(_.toDouble).max.toString :: Nil)
  val Avg = Val(x =>
    if (x.isEmpty) Nil else List((x.map(_.toDouble).sum / x.length).toString))
  val Unique = Val(_.distinct)
  val All = Val(x => x)
}
