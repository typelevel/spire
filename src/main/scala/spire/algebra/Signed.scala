package spire.algebra

import spire.math.{Fractional, Trig, Order, Real, Rational, Complex}

import scala.{ math => mth }
import scala.{ specialized => spec }


/**
 * A trait for things that have some notion of sign and the ability to ensure
 * something has a positive sign.
 */
trait Signed[@spec(Double, Float, Int, Long) A] {
  def sign(a: A): Sign = Sign(signum(a))
  def signum(a: A): Int = sign(a).toInt
  def abs(a: A): A
}

trait SignedLow {

  /// Constructs a `Signed[A]` for any `A` if it has both `Order` and `Ring`.
  implicit def OrderedRingIsSigned[A](implicit o: Order[A], r: Ring[A]): Signed[A] =
    new OrderedRingIsSigned[A] {
      val order = o
      val ring = r
    }
}

object Signed extends SignedLow {
  implicit object ByteIsSigned extends ByteIsSigned
  implicit object ShortIsSigned extends ShortIsSigned
  implicit object IntIsSigned extends IntIsSigned
  implicit object LongIsSigned extends LongIsSigned
  implicit object BigIntIsSigned extends BigIntIsSigned
  implicit object FloatIsSigned extends FloatIsSigned
  implicit object DoubleIsSigned extends DoubleIsSigned
  implicit object BigDecimalIsSigned extends BigDecimalIsSigned
  implicit object RationalIsSigned extends RationalIsSigned
  implicit object RealIsSigned extends RealIsSigned
  implicit def ComplexIsSigned[A:Fractional:Trig] = new ComplexIsSignedCls

  def apply[A](implicit s: Signed[A]): Signed[A] = s
}

final class SignedOps[@spec(Double, Float, Int, Long) A](lhs: A)(implicit s: Signed[A]) {
  def abs: A = s.abs(lhs)
  def sign: Sign = s.sign(lhs)
  def signum: Int = s.signum(lhs)
}

trait OrderedRingIsSigned[A] extends Signed[A] {
  def order: Order[A]
  def ring: Ring[A]

  override def sign(a: A): Sign = if (order.lteqv(a, ring.zero)) {
    if (order.eqv(a, ring.zero)) Zero else Negative
  } else {
    Positive
  }

  override def abs(a: A): A = if (order.lt(a, ring.zero)) {
    ring.negate(a)
  } else a
}

trait ByteIsSigned extends Signed[Byte] {
  override def signum(a: Byte): Int = a
  def abs(a: Byte): Byte = (if (a < 0) -a else a).toByte
}

trait ShortIsSigned extends Signed[Short] {
  override def signum(a: Short): Int = a
  def abs(a: Short): Short = (if (a < 0) -a else a).toShort
}

trait IntIsSigned extends Signed[Int] {
  override def signum(a: Int): Int = a
  def abs(a: Int): Int = if (a < 0) -a else a
}

trait LongIsSigned extends Signed[Long] {
  override def signum(a: Long): Int = if (a > 0) 1 else if (a < 0) -1 else 0
  def abs(a: Long): Long = if (a < 0L) -a else a
}

trait BigIntIsSigned extends Signed[BigInt] {
  override def signum(a: BigInt): Int = a.signum
  def abs(a: BigInt): BigInt = a.abs
}

trait FloatIsSigned extends Signed[Float] {
  override def signum(a: Float): Int = mth.signum(a).toInt
  def abs(a: Float): Float = if (a < 0.0f) -a else a
}

trait DoubleIsSigned extends Signed[Double] {
  // Note that math.signum(-1d) == -1 in 2.9.2.
  override def signum(a: Double): Int = if (a == 0.0) 0 else mth.signum(a).toInt
  def abs(a: Double): Double = if (a < 0.0) -a else a
}

trait BigDecimalIsSigned extends Signed[BigDecimal] {
  override def signum(a: BigDecimal): Int = a.signum
  def abs(a: BigDecimal): BigDecimal = a.abs
}

trait RationalIsSigned extends Signed[Rational] {
  override def sign(a: Rational): Sign = a.sign
  override def signum(a: Rational): Int = a.signum
  def abs(a: Rational): Rational = a.abs
}

trait RealIsSigned extends Signed[Real] {
  override def sign(a: Real): Sign = a.sign
  override def signum(a: Real): Int = a.signum
  def abs(a: Real): Real = a.abs
}

trait ComplexIsSigned[A] extends Signed[Complex[A]] {
  implicit def f:Fractional[A]
  implicit def t:Trig[A]

  override def signum(a: Complex[A]): Int = a.signum
  def abs(a: Complex[A]): Complex[A] = Complex[A](a.abs, f.zero)
}

class ComplexIsSignedCls[A](implicit val f:Fractional[A], val t:Trig[A])
extends ComplexIsSigned[A]
