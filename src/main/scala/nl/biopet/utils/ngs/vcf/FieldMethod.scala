package nl.biopet.utils.ngs.vcf

import scala.language.implicitConversions

object FieldMethod extends Enumeration {
  protected case class Val(doubleMethod: List[Double] => List[Double],
                           stringMethod: List[String] => List[String] = _ =>
                             throw new Exception("Not supported method"))
      extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  implicit def valToValue(x: Val): Value = x.asInstanceOf[Value]

  val Min = Val(x => if (x.isEmpty) Nil else x.min :: Nil)
  val Max = Val(x => if (x.isEmpty) Nil else x.max :: Nil)
  val Avg = Val(x => if (x.isEmpty) Nil else List(x.sum / x.length))
  val Unique = Val(_.distinct, _.distinct)
  val All = Val(x => x, x => x)
}
