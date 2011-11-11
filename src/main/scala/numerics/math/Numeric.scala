package numerics.math

import scala.math.{abs, ceil, floor}

/**
 * TODO
 * 1. better (e.g. specialized) version of Ordering
 * 2. OrderingOps, IntegralOps, FractionalOps, NumericOps
 * 3. LiteralOps? Literal conversions?
 * 4. Review operator symbols?
 * 5. Support for more operators?
 * 6. Start to worry about things like e.g. pow(BigInt, BigInt)
 */

/**
 *
 */
trait Ring[@specialized(Int,Long,Float,Double) A] extends ConvertableFrom[A] with ConvertableTo[A] {
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

trait EuclideanRing[@specialized(Int,Long,Float,Double) A] extends Ring[A] {
  def quot(a:A, b:A):A
  def mod(a:A, b:A):A
}

trait Field[@specialized(Float,Double) A] extends EuclideanRing[A] {
  def div(a:A, b:A):A
}

trait Integral[@specialized(Int,Long) A] extends EuclideanRing[A] with Ordering[A] {}

trait Fractional[@specialized(Float,Double) A] extends Field[A] with Ordering[A] {}

trait Numeric[@specialized(Int,Long,Float,Double) A] extends Field[A] with Ordering[A] {}

/**
 *
 */
trait IntIsRing extends Ring[Int] with ConvertableFromInt with ConvertableToInt {
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

trait IntIsEuclideanRing extends EuclideanRing[Int] with IntIsRing {
  def quot(a:Int, b:Int) = a / b
  def mod(a:Int, b:Int) = a % b
}

trait IntIsIntegral extends Integral[Int] with IntIsEuclideanRing with Ordering.IntOrdering {}

trait IntIsNumeric extends Numeric[Int] with IntIsEuclideanRing with Ordering.IntOrdering {
  def div(a:Int, b:Int) = a / b
}

/**
 *
 */
trait LongIsRing extends Ring[Long] with ConvertableFromLong with ConvertableToLong {
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

trait LongIsEuclideanRing extends EuclideanRing[Long] with LongIsRing {
  def quot(a:Long, b:Long) = a / b
  def mod(a:Long, b:Long) = a % b
}

trait LongIsIntegral extends Integral[Long] with LongIsEuclideanRing with Ordering.LongOrdering {}

trait LongIsNumeric extends Numeric[Long] with LongIsEuclideanRing with Ordering.LongOrdering {
  def div(a:Long, b:Long) = a / b
}


/**
 *
 */
trait FloatIsRing extends Ring[Float] with ConvertableFromFloat with ConvertableToFloat {
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

trait FloatIsEuclideanRing extends EuclideanRing[Float] with FloatIsRing {
  def quot(a:Float, b:Float) = {
    val d = a / b
    if (d < 0.0) ceil(d).toFloat else floor(d).toFloat
  }
  def mod(a:Float, b:Float) = a % b
}

trait FloatIsField extends Field[Float] with FloatIsEuclideanRing {
  def div(a:Float, b:Float) = a / b
}

trait FloatIsFractional extends Fractional[Float] with FloatIsField with Ordering.FloatOrdering {}

trait FloatIsNumeric extends Numeric[Float] with FloatIsField with Ordering.FloatOrdering {}


/**
 *
 */
trait DoubleIsRing extends Ring[Double] with ConvertableFromDouble with ConvertableToDouble {
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

trait DoubleIsEuclideanRing extends EuclideanRing[Double] with DoubleIsRing {
  def quot(a:Double, b:Double) = {
    val d = a / b
    if (d < 0.0) ceil(d) else floor(d)
  }
  def mod(a:Double, b:Double) = a % b
}

trait DoubleIsField extends Field[Double] with DoubleIsEuclideanRing {
  def div(a:Double, b:Double) = a / b
}

trait DoubleIsFractional extends Fractional[Double] with DoubleIsField with Ordering.DoubleOrdering {}

trait DoubleIsNumeric extends Numeric[Double] with DoubleIsField with Ordering.DoubleOrdering {}


/**
 *
 */
trait BigIntIsRing extends Ring[BigInt] with ConvertableFromBigInt with ConvertableToBigInt {
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

trait BigIntIsEuclideanRing extends EuclideanRing[BigInt] with BigIntIsRing {
  def quot(a:BigInt, b:BigInt) = a / b
  def mod(a:BigInt, b:BigInt) = a % b
}

trait BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing with Ordering.BigIntOrdering {}

trait BigIntIsNumeric extends Numeric[BigInt] with BigIntIsEuclideanRing with Ordering.BigIntOrdering {
  def div(a:BigInt, b:BigInt) = a / b
}


/**
 *
 */
trait BigDecimalIsRing extends Ring[BigDecimal] with ConvertableFromBigDecimal with ConvertableToBigDecimal {
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

trait BigDecimalIsEuclideanRing extends EuclideanRing[BigDecimal] with BigDecimalIsRing {
  def quot(a:BigDecimal, b:BigDecimal) = a.quot(b)
  def mod(a:BigDecimal, b:BigDecimal) = a % b
}

trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  def div(a:BigDecimal, b:BigDecimal) = a / b
}

trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField with Ordering.BigDecimalOrdering {}

trait BigDecimalIsNumeric extends Numeric[BigDecimal] with BigDecimalIsField with Ordering.BigDecimalOrdering {}


/**
 *
 */
trait RatOrdering extends Ordering[Rat] {
  def compare(a:Rat, b:Rat) = a.compare(b)
}

trait RatIsRing extends Ring[Rat] with ConvertableFromRat with ConvertableToRat {
  def abs(a:Rat): Rat = a.abs
  def eq(a:Rat, b:Rat): Boolean = a == b
  def minus(a:Rat, b:Rat): Rat = a - b
  def negate(a:Rat): Rat = -a
  def neq(a:Rat, b:Rat): Boolean = a != b
  def one: Rat = Rat.one
  def plus(a:Rat, b:Rat): Rat = a + b
  def times(a:Rat, b:Rat): Rat = a * b
  def zero: Rat = Rat.zero
}

trait RatIsEuclideanRing extends EuclideanRing[Rat] with RatIsRing {
  def quot(a:Rat, b:Rat) = a.quot(b)
  def mod(a:Rat, b:Rat) = a % b
}

trait RatIsField extends Field[Rat] with RatIsEuclideanRing {
  def div(a:Rat, b:Rat) = a / b
}

trait RatIsFractional extends Fractional[Rat] with RatIsField with RatOrdering {}

trait RatIsNumeric extends Numeric[Rat] with RatIsField with RatOrdering {}


/**
 *
 */
trait OrderingOps[@specialized(Int,Long,Float,Double) A] {
  val lhs:A
  val o:Ordering[A]

  def <(rhs:A) = o.lt(lhs, rhs)
  def <=(rhs:A) = o.lteq(lhs, rhs)
  def >(rhs:A) = o.gt(lhs, rhs)
  def >=(rhs:A) = o.gteq(lhs, rhs)
}

trait RingOps[@specialized(Int,Long,Float,Double) A] {
  val lhs:A
  val n:Ring[A]

  def ===(rhs:A) = n.eq(lhs, rhs)
  def !==(rhs:A) = n.neq(lhs, rhs)

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

trait EuclideanRingOps[@specialized(Int,Long,Float,Double) A] {
  val lhs:A
  val n:EuclideanRing[A]

  def /~(rhs:A) = n.quot(lhs, rhs)
  def %(rhs:A) = n.mod(lhs, rhs)
  def /%(rhs:A) = (n.quot(lhs, rhs), n.mod(lhs, rhs))
}

trait FieldOps[@specialized(Int,Long,Float,Double) A] {
  val lhs:A
  val n:Field[A]

  def /(rhs:A) = n.div(lhs, rhs)
}

final class OrderingOpsImpl[A:Ordering](val lhs:A) extends OrderingOps[A] {
  val o = implicitly[Ordering[A]]
}

final class RingOpsImpl[A:Ring](val lhs:A) extends RingOps[A] {
  val n = implicitly[Ring[A]]
}

final class EuclideanRingOpsImpl[A:EuclideanRing](val lhs:A) extends EuclideanRingOps[A] {
  val n = implicitly[EuclideanRing[A]]
}

final class FieldOpsImpl[A:Field](val lhs:A) extends FieldOps[A] {
  val n = implicitly[Field[A]]
}


/**
 *
 */
object Ring {
  implicit object IntIsRing extends IntIsRing
  implicit object LongIsRing extends LongIsRing
  implicit object FloatIsRing extends FloatIsRing
  implicit object DoubleIsRing extends DoubleIsRing
  implicit object BigIntIsRing extends BigIntIsRing
  implicit object BigDecimalIsRing extends BigDecimalIsRing
  implicit object RatIsRing extends RatIsRing
}

object EuclideanRing {
  implicit object IntIsEuclideanRing extends IntIsEuclideanRing
  implicit object LongIsEuclideanRing extends LongIsEuclideanRing
  implicit object FloatIsEuclideanRing extends FloatIsEuclideanRing
  implicit object DoubleIsEuclideanRing extends DoubleIsEuclideanRing
  implicit object BigIntIsEuclideanRing extends BigIntIsEuclideanRing
  implicit object BigDecimalIsEuclideanRing extends BigDecimalIsEuclideanRing
  implicit object RatIsEuclideanRing extends RatIsEuclideanRing
}

object Field {
  implicit object FloatIsField extends FloatIsField
  implicit object DoubleIsField extends DoubleIsField
  implicit object BigDecimalIsField extends BigDecimalIsField
  implicit object RatIsField extends RatIsField
}

object Integral {
  implicit object IntIsIntegral extends IntIsIntegral
  implicit object LongIsIntegral extends LongIsIntegral
  implicit object BigIntIsIntegral extends BigIntIsIntegral
}

object Fractional {
  implicit object FloatIsFractional extends FloatIsFractional
  implicit object DoubleIsFractional extends DoubleIsFractional
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional
  implicit object RatIsFractional extends RatIsFractional
}

object Numeric {
  implicit object IntIsNumeric extends IntIsNumeric
  implicit object LongIsNumeric extends LongIsNumeric
  implicit object FloatIsNumeric extends FloatIsNumeric
  implicit object DoubleIsNumeric extends DoubleIsNumeric
  implicit object BigIntIsNumeric extends BigIntIsNumeric
  implicit object BigDecimalIsNumeric extends BigDecimalIsNumeric
  implicit object RatIsNumeric extends RatIsNumeric
}


/**
 *
 */
object Implicits {
  implicit def orderingOps[@specialized(Int, Long, Float, Double) A:Ordering](a:A) = new OrderingOpsImpl(a)
  implicit def ringOps[@specialized(Int, Long, Float, Double) A:Ring](a:A) = new RingOpsImpl(a)
  implicit def euclideanRingOps[@specialized(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOpsImpl(a)
  implicit def fieldOps[@specialized(Float, Double) A:Field](a:A) = new FieldOpsImpl(a)


  def ring[@specialized(Int, Long, Float, Double) A:Ring]:Ring[A] = implicitly[Ring[A]]
  def euclideanRing[@specialized(Int, Long, Float, Double) A:EuclideanRing]:EuclideanRing[A] = implicitly[EuclideanRing[A]]
  def field[@specialized(Float, Double) A:Field]:Field[A] = implicitly[Field[A]]
  def integral[@specialized(Int, Long) A:Integral]:Integral[A] = implicitly[Integral[A]]
  def fractional[@specialized(Float, Double) A:Fractional]:Fractional[A] = implicitly[Fractional[A]]
  def numeric[@specialized(Int, Long, Float, Double) A:Numeric]:Numeric[A] = implicitly[Numeric[A]]
}
