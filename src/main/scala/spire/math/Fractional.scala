package spire.math

import spire.algebra._
import spire.macros._

import language.experimental.macros
import scala.{specialized => spec}

trait Fractional[@spec(Float,Double) A] extends Field[A] with NRoot[A] with Integral[A] {
  def ceil(a:A):A
  def floor(a:A):A
}

class FractionalOps[A](lhs:A)(implicit ev:Fractional[A]) {
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
}

object Fractional {
  implicit object FloatIsFractional extends FloatIsFractional
  implicit object DoubleIsFractional extends DoubleIsFractional
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional
  implicit def RationalIsFractional(implicit ctx: ApproximationContext[Rational] =
      ApproximationContext(Rational(1, 1000000000))) = new RationalIsFractional {
    val context = ctx
  }
  implicit object RealIsFractional extends RealIsFractional

  @inline final def apply[A](implicit ev:Fractional[A]) = ev
}

trait FloatIsFractional extends Fractional[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatOrder with FloatIsSigned {
  override def fromInt(n: Int): Float = n
  def ceil(a:Float) = scala.math.ceil(a).toFloat
  def floor(a:Float) = scala.math.floor(a).toFloat
}

trait DoubleIsFractional extends Fractional[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleOrder with DoubleIsSigned {
  override def fromInt(n: Int): Double = n
  def ceil(a:Double) = scala.math.ceil(a)
  def floor(a:Double) = scala.math.floor(a)
}


/**
 * A generic implementation of ceil/floor that can be mixed in.
 */
trait GenericCeilAndFloor[A] { self: Fractional[A] =>
  def ceil(a: A) = {
    val i = self.fromBigInt(self.toBigInt(a))

    if (i == a || self.lt(a, self.fromInt(0))) {
      i
    } else {
      self.plus(i, self.fromInt(1))
    }
  }

  def floor(a: A) = {
    val i = self.fromBigInt(self.toBigInt(a))

    if (i == a || self.gt(a, self.fromInt(0))) {
      i
    } else {
      self.plus(i, self.fromInt(1))
    }
  }
}

trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalOrder with BigDecimalIsSigned {
  override def fromInt(n: Int): BigDecimal = super[ConvertableToBigDecimal].fromInt(n)
  def ceil(a:BigDecimal) = {
    val (q, r) = a /% 1
    if (r > 0) q + BigDecimal(1) else q
  }
  def floor(a:BigDecimal) = a.quot(1)
}

trait RationalIsFractional extends Fractional[Rational] with RationalIsField
with RationalIsNRoot with ConvertableFromRational with ConvertableToRational
with RationalOrder with RationalIsSigned with GenericCeilAndFloor[Rational] {
  override def fromInt(n: Int): Rational = super[ConvertableToRational].fromInt(n)
}


trait RealIsFractional extends Fractional[Real] with RealIsField
with RealIsNRoot with ConvertableFromReal with ConvertableToReal
with RealOrder with RealIsSigned with GenericCeilAndFloor[Real] {
  override def fromInt(n: Int): Real = super[ConvertableToReal].fromInt(n)
}
