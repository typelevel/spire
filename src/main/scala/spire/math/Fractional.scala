package spire.math

import spire.algebra._
import spire.macrosk.Ops

import scala.{specialized => spec}
import java.lang.Math

trait Fractional[@spec(Float,Double) A] extends Field[A] with NRoot[A] with Integral[A] {
  def ceil(a:A):A
  def floor(a:A):A
  def round(a:A):A
}

class FractionalOps[A](lhs:A)(implicit ev:Fractional[A]) {
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]

  // TODO: these should really be macro-ized somehow
  def +(rhs: Double): A = ev.plus(lhs, ev.fromDouble(rhs))
  def -(rhs: Double): A = ev.minus(lhs, ev.fromDouble(rhs))
  def *(rhs: Double): A = ev.times(lhs, ev.fromDouble(rhs))
  def /(rhs: Double): A = ev.div(lhs, ev.fromDouble(rhs))
  def /~(rhs: Double): A = ev.quot(lhs, ev.fromDouble(rhs))
  def %(rhs: Double): A = ev.mod(lhs, ev.fromDouble(rhs))
  def /%(rhs: Double): (A, A) = ev.quotmod(lhs, ev.fromDouble(rhs))
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
  def ceil(a:Float) = Math.ceil(a).toFloat
  def floor(a:Float) = Math.floor(a).toFloat
  def round(a:Float) = spire.math.round(a)
}

trait DoubleIsFractional extends Fractional[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleOrder with DoubleIsSigned {
  override def fromInt(n: Int): Double = n
  def ceil(a:Double) = Math.ceil(a)
  def floor(a:Double) = Math.floor(a)
  def round(a:Double) = spire.math.round(a)
}


/**
 * A generic implementation of ceil/floor that can be mixed in.
 */
trait GenericCeilAndFloor[A] { self: Fractional[A] =>
  import self._
  def ceil(a: A) = {
    val q = quot(a, one)
    if (q == a || lt(a, zero)) q else plus(q, one)
  }

  def floor(a: A) = {
    val q = quot(a, one)
    if (q == a || gt(a, zero)) q else plus(q, one)
  }

  def round(a: A) = {
    val r = mod(a, one)
    val q = minus(a, r)
    if (lt(r, zero)) {
      if (q == a || lt(times(r, fromInt(-2)), one)) q else minus(q, one)
    } else {
      if (q == a || lt(times(r, fromInt(2)), one)) q else plus(q, one)
    }
  }
}

trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalOrder with BigDecimalIsSigned with GenericCeilAndFloor[BigDecimal] {
  override def fromInt(n: Int): BigDecimal =
    super[ConvertableToBigDecimal].fromInt(n)
}

trait RationalIsFractional extends Fractional[Rational] with RationalIsField
with RationalIsNRoot with ConvertableFromRational with ConvertableToRational
with RationalOrder with RationalIsSigned with GenericCeilAndFloor[Rational] {
  override def fromInt(n: Int): Rational =
    super[ConvertableToRational].fromInt(n)
}


trait RealIsFractional extends Fractional[Real] with RealIsField
with RealIsNRoot with ConvertableFromReal with ConvertableToReal
with RealOrder with RealIsSigned with GenericCeilAndFloor[Real] {
  override def fromInt(n: Int): Real = super[ConvertableToReal].fromInt(n)
}
