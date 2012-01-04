package numerics.math

import scala.{specialized => spec}
import scala.math.{abs, ceil, floor}

trait Ring[@spec(Int,Long,Float,Double) A] extends ConvertableFrom[A] with ConvertableTo[A] {
  def abs(a:A):A
  def eq(a:A, b:A):Boolean
  def minus(a:A, b:A):A
  def negate(a:A):A
  def neq(a:A, b:A):Boolean
  def one:A
  def plus(a:A, b:A):A
  def times(a:A, b:A):A
  def zero:A
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
}

trait IntIsRing extends Ring[Int]
with ConvertableFromInt with ConvertableToInt {
  def abs(a:Int): Int = scala.math.abs(a)
  def eq(a:Int, b:Int): Boolean = a == b
  def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def neq(a:Int, b:Int): Boolean = a != b
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0
}

trait LongIsRing extends Ring[Long]
with ConvertableFromLong with ConvertableToLong {
  def abs(a:Long): Long = scala.math.abs(a)
  def eq(a:Long, b:Long): Boolean = a == b
  def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def neq(a:Long, b:Long): Boolean = a != b
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
}

trait FloatIsRing extends Ring[Float]
with ConvertableFromFloat with ConvertableToFloat {
  def abs(a:Float): Float = scala.math.abs(a)
  def eq(a:Float, b:Float): Boolean = a == b
  def minus(a:Float, b:Float): Float = a - b
  def negate(a:Float): Float = -a
  def neq(a:Float, b:Float): Boolean = a != b
  def one: Float = 1.0F
  def plus(a:Float, b:Float): Float = a + b
  def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
}

trait DoubleIsRing extends Ring[Double]
with ConvertableFromDouble with ConvertableToDouble {
  def abs(a:Double): Double = scala.math.abs(a)
  def eq(a:Double, b:Double): Boolean = a == b
  def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def neq(a:Double, b:Double): Boolean = a != b
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
  def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0
}

trait BigIntIsRing extends Ring[BigInt]
with ConvertableFromBigInt with ConvertableToBigInt {
  def abs(a:BigInt): BigInt = a.abs
  def eq(a:BigInt, b:BigInt): Boolean = a == b
  def minus(a:BigInt, b:BigInt): BigInt = a - b
  def negate(a:BigInt): BigInt = -a
  def neq(a:BigInt, b:BigInt): Boolean = a != b
  def one: BigInt = BigInt(1)
  def plus(a:BigInt, b:BigInt): BigInt = a + b
  def times(a:BigInt, b:BigInt): BigInt = a * b
  def zero: BigInt = BigInt(0)
}

trait BigDecimalIsRing extends Ring[BigDecimal] 
with ConvertableFromBigDecimal with ConvertableToBigDecimal {
  def abs(a:BigDecimal): BigDecimal = a.abs
  def eq(a:BigDecimal, b:BigDecimal): Boolean = a == b
  def minus(a:BigDecimal, b:BigDecimal): BigDecimal = a - b
  def negate(a:BigDecimal): BigDecimal = -a
  def neq(a:BigDecimal, b:BigDecimal): Boolean = a != b
  def one: BigDecimal = BigDecimal(1.0)
  def plus(a:BigDecimal, b:BigDecimal): BigDecimal = a + b
  def times(a:BigDecimal, b:BigDecimal): BigDecimal = a * b
  def zero: BigDecimal = BigDecimal(0.0)
}

trait RationalIsRing extends Ring[Rational]
with ConvertableFromRational with ConvertableToRational {
  def abs(a:Rational): Rational = a.abs
  def eq(a:Rational, b:Rational): Boolean = a == b
  def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def neq(a:Rational, b:Rational): Boolean = a != b
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
}
