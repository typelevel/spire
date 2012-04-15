package spire.math

import spire.algebra._

import scala.{specialized => spec}

/**
 * TODO
 * 3. LiteralOps? Literal conversions?
 * 4. Review operator symbols?
 * 5. Support for more operators?
 * 6. Start to worry about things like e.g. pow(BigInt, BigInt)
 */

trait Numeric[@spec(Int,Long,Float,Double) A] extends FieldWithNRoot[A]
with ConvertableFrom[A] with ConvertableTo[A] with Order[A] with Signed[A]

object Numeric {
  implicit object IntIsNumeric extends IntIsNumeric
  implicit object LongIsNumeric extends LongIsNumeric
  implicit object FloatIsNumeric extends FloatIsNumeric
  implicit object DoubleIsNumeric extends DoubleIsNumeric
  implicit object BigIntIsNumeric extends BigIntIsNumeric
  implicit object BigDecimalIsNumeric extends BigDecimalIsNumeric
  implicit def RationalIsNumeric(implicit ctx: ApproximationContext[Rational] =
      ApproximationContext(Rational(1, 1000000000))) = new RationalIsNumeric {
    val context = ctx
  }
  implicit object RealIsNumeric extends RealIsNumeric
  implicit def complexIsNumeric[A:Fractional] = new ComplexIsNumeric

  def apply[A](implicit e:Numeric[A]):Numeric[A] = e
}

trait IntIsNumeric extends Numeric[Int] with IntIsEuclideanRing with IntIsNRoot
with ConvertableFromInt with ConvertableToInt with IntOrder with IntIsSigned {
  override def fromInt(n: Int): Int = n
  def div(a:Int, b:Int) = a / b
}

trait LongIsNumeric extends Numeric[Long] with LongIsEuclideanRing with LongIsNRoot
with ConvertableFromLong with ConvertableToLong with LongOrder with LongIsSigned {
  override def fromInt(n: Int): Long = n
  def div(a:Long, b:Long) = a / b
}

trait BigIntIsNumeric extends Numeric[BigInt] with BigIntIsEuclideanRing
with BigIntIsNRoot with ConvertableFromBigInt with ConvertableToBigInt
with BigIntOrder with BigIntIsSigned {
  override def fromInt(n: Int): BigInt = super[ConvertableToBigInt].fromInt(n)
  def div(a:BigInt, b:BigInt) = a / b
}

trait FloatIsNumeric extends Numeric[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatOrder with FloatIsSigned {
  override def fromInt(n: Int): Float = n
}

trait DoubleIsNumeric extends Numeric[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleOrder with DoubleIsSigned {
  override def fromInt(n: Int): Double = n
}

trait BigDecimalIsNumeric extends Numeric[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalOrder with BigDecimalIsSigned {
  override def fromInt(n: Int): BigDecimal = super[ConvertableToBigDecimal].fromInt(n)
}

trait RationalIsNumeric extends Numeric[Rational] with RationalIsField
with RationalIsNRoot with ConvertableFromRational with ConvertableToRational
with RationalOrder with RationalIsSigned {
  override def fromInt(n: Int): Rational = super[ConvertableToRational].fromInt(n)
}

trait RealIsNumeric extends Numeric[Real] with RealIsField with RealIsNRoot
with ConvertableFromReal with ConvertableToReal with RealOrder with RealIsSigned {
  override def fromInt(n: Int): Real = super[ConvertableToReal].fromInt(n)
}


class ComplexIsNumeric[A](implicit val f:Fractional[A])
extends ComplexIsField[A] with Numeric[Complex[A]] with ComplexEq[A]
with NRoot[Complex[A]] with ConvertableFromComplex[A] with ConvertableToComplex[A]
with Order[Complex[A]] with ComplexIsSigned[A] {
  override def fromInt(n: Int): Complex[A] = super[ConvertableToComplex].fromInt(n)

  override def nroot(a: Complex[A], n: Int) = sys.error("not implemented")
  override def gt(x:Complex[A], y:Complex[A]) = sys.error("undefined")
  override def gteq(x:Complex[A], y:Complex[A]) = sys.error("undefined")
  override def lt(x:Complex[A], y:Complex[A]) = sys.error("undefined")
  override def lteq(x:Complex[A], y:Complex[A]) = sys.error("undefined")
  def compare(x:Complex[A], y:Complex[A]) = sys.error("undefined")
}
