package numerics.math

import scala.{specialized => spec}

/**
 * TODO
 * 3. LiteralOps? Literal conversions?
 * 4. Review operator symbols?
 * 5. Support for more operators?
 * 6. Start to worry about things like e.g. pow(BigInt, BigInt)
 */

trait Numeric[@spec(Int,Long,Float,Double) A] extends Field[A] with Order[A]

object Numeric {
  implicit object IntIsNumeric extends IntIsNumeric
  implicit object LongIsNumeric extends LongIsNumeric
  implicit object FloatIsNumeric extends FloatIsNumeric
  implicit object DoubleIsNumeric extends DoubleIsNumeric
  implicit object BigIntIsNumeric extends BigIntIsNumeric
  implicit object BigDecimalIsNumeric extends BigDecimalIsNumeric
  implicit object RationalIsNumeric extends RationalIsNumeric
}

trait IntIsNumeric extends Numeric[Int] with IntIsEuclideanRing with IntOrder {
  def div(a:Int, b:Int) = a / b
}

trait LongIsNumeric extends Numeric[Long] with LongIsEuclideanRing with LongOrder {
  def div(a:Long, b:Long) = a / b
}

trait BigIntIsNumeric extends Numeric[BigInt] with BigIntIsEuclideanRing with BigIntOrder {
  def div(a:BigInt, b:BigInt) = a / b
}

trait FloatIsNumeric extends Numeric[Float] with FloatIsField with FloatOrder
trait DoubleIsNumeric extends Numeric[Double] with DoubleIsField with DoubleOrder
trait BigDecimalIsNumeric extends Numeric[BigDecimal] with BigDecimalIsField with BigDecimalOrder
trait RationalIsNumeric extends Numeric[Rational] with RationalIsField with RationalOrder
trait RealIsNumeric extends Numeric[Real] with RealIsField with RealOrder
