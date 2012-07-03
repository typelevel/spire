package spire.algebra

import annotation.tailrec
import language.experimental.macros
import scala.{specialized => spec}
import scala.math.{abs, ceil, floor, pow => mpow}

import spire.math._
import spire.macros._

/**
 * Ring represents a set (A) that is a group over addition (+) and a monoid
 * over multiplication (*).
 *
 * Ring implements some methods (for example fromInt) in terms of other more
 * fundamental methods (zero, one and plus). Where possible, these methods
 * should be overridden by more efficient implementations.
 */
trait Ring[@spec(Int,Long,Float,Double) A] {
  self =>

  def zero:A
  def one:A
  def negate(a:A):A
  def plus(a:A, b:A):A
  def times(a:A, b:A):A

  def additive:Group[A] = new AdditiveGroup[A]()(this)
  def multiplicative:Monoid[A] = new MultiplicativeMonoid[A]()(this)

  def minus(a:A, b:A):A = plus(a, negate(b))

  def pow(a:A, n:Int):A =
    if (n < 0) sys.error("illegal exponent: %s" format n)
    else _pow(a, n, one)

  def fromInt(n: Int): A =
    if (n < 0) _fromInt(negate(one), -n, zero)
    else _fromInt(one, n, zero)

  @tailrec protected[this] final def _pow(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _pow(times(a, a), n / 2, times(sofar, a))
    else _pow(times(a, a), n / 2, sofar)

  @tailrec private def _fromInt(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _fromInt(plus(a, a), n / 2, plus(sofar, a))
    else _fromInt(plus(a, a), n / 2, sofar)
}

final class RingOps[@spec(Int,Long,Float,Double) A](lhs:A)(implicit ev:Ring[A]) {
  def unary_-() = macro Macros.negate[A]

  def -(rhs:A) = macro Macros.minus[A]
  def +(rhs:A) = macro Macros.plus[A]
  def *(rhs:A) = macro Macros.times[A]

  def pow(rhs:Int) = macro Macros.pow[A]
  def **(rhs:Int) = macro Macros.pow[A]
}

object Ring {
  implicit object ByteIsRing extends ByteIsRing
  implicit object ShortIsRing extends ShortIsRing
  implicit object IntIsRing extends IntIsRing
  implicit object LongIsRing extends LongIsRing
  implicit object FloatIsRing extends FloatIsRing
  implicit object DoubleIsRing extends DoubleIsRing
  implicit object BigIntIsRing extends BigIntIsRing
  implicit object BigDecimalIsRing extends BigDecimalIsRing
  implicit object RationalIsRing extends RationalIsRing
  implicit def complexIsRing[A:Fractional:Trig] = new ComplexIsRingCls
  implicit object RealIsRing extends RealIsRing

  def apply[A](implicit r:Ring[A]):Ring[A] = r
}

trait ByteIsRing extends Ring[Byte] {
  override def minus(a:Byte, b:Byte): Byte = (a - b).toByte
  def negate(a:Byte): Byte = (-a).toByte
  def one: Byte = 1.toByte
  def plus(a:Byte, b:Byte): Byte = (a + b).toByte
  override def pow(a: Byte, b:Int): Byte = mpow(a, b).toByte
  override def times(a:Byte, b:Byte): Byte = (a * b).toByte
  def zero: Byte = 0.toByte
  
  override def fromInt(n: Int): Byte = n.toByte
}

trait ShortIsRing extends Ring[Short] {
  override def minus(a:Short, b:Short): Short = (a - b).toShort
  def negate(a:Short): Short = (-a).toShort
  def one: Short = 1.toShort
  def plus(a:Short, b:Short): Short = (a + b).toShort
  override def pow(a: Short, b:Int): Short = mpow(a, b).toShort
  override def times(a:Short, b:Short): Short = (a * b).toShort
  def zero: Short = 0.toShort
  
  override def fromInt(n: Int): Short = n.toShort
}

trait IntIsRing extends Ring[Int] {
  override def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  override def pow(a:Int, b:Int): Int = mpow(a, b).toInt
  override def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0

  override def fromInt(n: Int): Int = n
}

trait LongIsRing extends Ring[Long] {
  override def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  override def pow(a: Long, b:Int): Long = b match {
    case 0 => 1
    case 1 => a
    case 2 => a * a
    case 3 => a * a * a
    case _ =>
      if (b > 0) {
        val e = b >> 1
        val c = if ((b & 1) == 1) a else 1
        c * pow(a, e) * pow(a, e)
      } else {
        0
      }
  }
  override def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
  
  override def fromInt(n: Int): Long = n
}

trait FloatIsRing extends Ring[Float] {
  override def minus(a:Float, b:Float): Float = a - b
  def negate(a:Float): Float = -a
  def one: Float = 1.0F
  def plus(a:Float, b:Float): Float = a + b
  override def pow(a:Float, b:Int): Float = mpow(a, b).toFloat
  override def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
  
  override def fromInt(n: Int): Float = n
}

trait DoubleIsRing extends Ring[Double] {
  override def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
  override def pow(a:Double, b:Int): Double = mpow(a, b)
  override def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0

  override def fromInt(n: Int): Double = n
}

trait BigIntIsRing extends Ring[BigInt] {
  override def minus(a:BigInt, b:BigInt): BigInt = a - b
  def negate(a:BigInt): BigInt = -a
  def one: BigInt = BigInt(1)
  def plus(a:BigInt, b:BigInt): BigInt = a + b
  override def pow(a:BigInt, b:Int): BigInt = a pow b
  override def times(a:BigInt, b:BigInt): BigInt = a * b
  def zero: BigInt = BigInt(0)
  
  override def fromInt(n: Int): BigInt = BigInt(n)
}

trait BigDecimalIsRing extends Ring[BigDecimal] {
  override def minus(a:BigDecimal, b:BigDecimal): BigDecimal = a - b
  def negate(a:BigDecimal): BigDecimal = -a
  def one: BigDecimal = BigDecimal(1.0)
  def plus(a:BigDecimal, b:BigDecimal): BigDecimal = a + b
  override def pow(a:BigDecimal, b:Int): BigDecimal = a.pow(b)
  override def times(a:BigDecimal, b:BigDecimal): BigDecimal = a * b
  def zero: BigDecimal = BigDecimal(0.0)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
}

trait RationalIsRing extends Ring[Rational] {
  override def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  override def pow(a:Rational, b:Int): Rational = a.pow(b)
  override def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
  
  override def fromInt(n: Int): Rational = Rational(n)
}

trait ComplexIsRing[A] extends Ring[Complex[A]] {
  implicit def f:Fractional[A]
  implicit def t:Trig[A]

  override def minus(a:Complex[A], b:Complex[A]): Complex[A] = a - b
  def negate(a:Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one(f, t)
  def plus(a:Complex[A], b:Complex[A]): Complex[A] = a + b
  override def pow(a:Complex[A], b:Int):Complex[A] = a.pow(Complex(f.fromInt(b), f.zero))
  override def times(a:Complex[A], b:Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero(f, t)

  override def fromInt(n: Int): Complex[A] = Complex(f.fromInt(n), f.zero)
}

trait RealIsRing extends Ring[Real] {
  override def minus(a: Real, b: Real): Real = a - b
  def negate(a: Real): Real = -a
  def one: Real = Real(1)
  def plus(a: Real, b: Real): Real = a + b
  override def pow(a: Real, b: Int): Real = a pow b
  override def times(a: Real, b: Real): Real = a * b
  def zero: Real = Real(0)
  
  override def fromInt(n: Int): Real = Real(n)
}

class ComplexIsRingCls[A](implicit val f:Fractional[A], val t:Trig[A])
extends ComplexIsRing[A]
