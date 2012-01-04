package numerics.math

import scala.{specialized => spec}
import scala.math.{abs, ceil, floor}

trait Ring[@spec(Int,Long,Float,Double) A] extends Eq[A]
with ConvertableFrom[A] with ConvertableTo[A] {
  self =>

  def abs(a:A):A
  def minus(a:A, b:A):A
  def negate(a:A):A
  def one:A
  def plus(a:A, b:A):A
  def times(a:A, b:A):A
  def zero:A

  def additiveMonoid:Monoid[A] = new Monoid[A] {
    def identity = self.zero
    def op(x:A, y:A) = self.plus(x, y)
    def equiv(x:A, y:A) = self.equiv(x, y)
    def nequiv(x:A, y:A) = self.nequiv(x, y)
  }

  def multiplicativeMonoid = new Monoid[A] {
    def identity = self.one
    def op(x:A, y:A) = self.times(x, y)
    def equiv(x:A, y:A) = self.equiv(x, y)
    def nequiv(x:A, y:A) = self.nequiv(x, y)
  }
}

trait RingOps[@spec(Int,Long,Float,Double) A] {
  val lhs:A
  val n:Ring[A]

  def abs = n.abs(lhs)
  def unary_- = n.negate(lhs)

  def -(rhs:A) = n.minus(lhs, rhs)
  def +(rhs:A) = n.plus(lhs, rhs)
  def *(rhs:A) = n.times(lhs, rhs)

  def toInt = n.toInt(lhs)
  def toLong = n.toLong(lhs)
  def toFloat = n.toFloat(lhs)
  def toDouble = n.toDouble(lhs)
  def toBigInt = n.toBigInt(lhs)
  def toBigDecimal = n.toBigDecimal(lhs)
}

object Ring {
  implicit object IntIsRing extends IntIsRing
  implicit object LongIsRing extends LongIsRing
  implicit object FloatIsRing extends FloatIsRing
  implicit object DoubleIsRing extends DoubleIsRing
  implicit object BigIntIsRing extends BigIntIsRing
  implicit object BigDecimalIsRing extends BigDecimalIsRing
  implicit object RationalIsRing extends RationalIsRing
  implicit def complexIsRing[A:Fractional] = new ComplexIsRing
}

trait IntIsRing extends Ring[Int] with IntEq
with ConvertableFromInt with ConvertableToInt {
  def abs(a:Int): Int = scala.math.abs(a)
  def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0
}

trait LongIsRing extends Ring[Long] with LongEq
with ConvertableFromLong with ConvertableToLong {
  def abs(a:Long): Long = scala.math.abs(a)
  def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
}

trait FloatIsRing extends Ring[Float] with FloatEq
with ConvertableFromFloat with ConvertableToFloat {
  def abs(a:Float): Float = scala.math.abs(a)
  def minus(a:Float, b:Float): Float = a - b
  def negate(a:Float): Float = -a
  def one: Float = 1.0F
  def plus(a:Float, b:Float): Float = a + b
  def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
}

trait DoubleIsRing extends Ring[Double] with DoubleEq
with ConvertableFromDouble with ConvertableToDouble {
  def abs(a:Double): Double = scala.math.abs(a)
  def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
  def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0
}

trait BigIntIsRing extends Ring[BigInt] with BigIntEq
with ConvertableFromBigInt with ConvertableToBigInt {
  def abs(a:BigInt): BigInt = a.abs
  def minus(a:BigInt, b:BigInt): BigInt = a - b
  def negate(a:BigInt): BigInt = -a
  def one: BigInt = BigInt(1)
  def plus(a:BigInt, b:BigInt): BigInt = a + b
  def times(a:BigInt, b:BigInt): BigInt = a * b
  def zero: BigInt = BigInt(0)
}

trait BigDecimalIsRing extends Ring[BigDecimal] with BigDecimalEq
with ConvertableFromBigDecimal with ConvertableToBigDecimal {
  def abs(a:BigDecimal): BigDecimal = a.abs
  def minus(a:BigDecimal, b:BigDecimal): BigDecimal = a - b
  def negate(a:BigDecimal): BigDecimal = -a
  def one: BigDecimal = BigDecimal(1.0)
  def plus(a:BigDecimal, b:BigDecimal): BigDecimal = a + b
  def times(a:BigDecimal, b:BigDecimal): BigDecimal = a * b
  def zero: BigDecimal = BigDecimal(0.0)
}

trait RationalIsRing extends Ring[Rational] with RationalEq
with ConvertableFromRational with ConvertableToRational {
  def abs(a:Rational): Rational = a.abs
  def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
}

class ComplexIsRing[A](implicit val f:Fractional[A])
extends ComplexEq[A] with Ring[Complex[A]] with ConvertableFromComplex[A]
with ConvertableToComplex[A] {
  def abs(a:Complex[A]): Complex[A] = Complex(a.abs, f.zero)(f)
  def minus(a:Complex[A], b:Complex[A]): Complex[A] = a - b
  def negate(a:Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one(f)
  def plus(a:Complex[A], b:Complex[A]): Complex[A] = a + b
  def times(a:Complex[A], b:Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero(f)
}
