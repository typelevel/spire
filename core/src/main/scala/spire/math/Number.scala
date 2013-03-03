package spire.math

import scala.math.{BigDecimal, ScalaNumber, ScalaNumericConversions}
import java.lang.Math

import spire.algebra._
import spire.algebra.std.bigDecimal._

// TODO: implement RationalNumber.
// TODO: implement toNumber and fromNumber in ConvertableTo/From.
// TODO: pow() is hairy; should support more cases and generate better errors
// TODO: decide what should be public/private

case class NumberOutOfRange(msg: String) extends Exception(msg)
case class InvalidNumber(msg: String) extends Exception(msg)

/**
 * Convenient apply and implicits for Numbers
 */
object Number extends NumberInstances {

  final val zero = Number(0)
  final val one = Number(1)

  def apply(n: Byte): Number = IntNumber(SafeLong(n))
  def apply(n: Short): Number = IntNumber(SafeLong(n))
  def apply(n: Int): Number = IntNumber(SafeLong(n))
  def apply(n: Long): Number = IntNumber(SafeLong(n))
  def apply(n: BigInt): Number = IntNumber(SafeLong(n))
  def apply(n: SafeLong): Number = IntNumber(n)
  def apply(n: Double): Number = FloatNumber(n)
  def apply(n: BigDecimal): Number = DecimalNumber(n)

  object implicits {
  }

  def apply(s: String): Number = try {
    Number(SafeLong(s))
  } catch {
    case _: Exception => Number(BigDecimal(s))
  }

  implicit def byteToNumber(n: Byte) = Number(n)
  implicit def shortToNumber(n: Short) = Number(n)
  implicit def intToNumber(n: Int) = Number(n)
  implicit def longToNumber(n: Long) = Number(n)
  implicit def bigIntToNumber(n: BigInt) = Number(n)
  implicit def doubleToNumber(n: Double) = Number(n)
  implicit def bigDecimalToNumber(n: BigDecimal) = Number(n)
  implicit def safeLongToNumber(n: SafeLong) = Number(n)
  implicit def naturalToNumber(n: Natural) = Number(n.toBigInt)
  // TODO: Rational? Real? Complex?

  private[math] val minInt = SafeLong(Int.MinValue)
  private[math] val maxInt = SafeLong(Int.MaxValue)

  private[math] val minLong = SafeLong(Long.MinValue)
  private[math] val maxLong = SafeLong(Long.MaxValue)

  private[math] val minDouble = BigDecimal(Double.MinValue)
  private[math] val maxDouble = BigDecimal(Double.MaxValue)
}

sealed trait Number extends ScalaNumericConversions {
  def abs: Number
  def signum: Int

  def withinInt: Boolean
  def withinLong: Boolean
  def withinDouble: Boolean

  def canBeInt: Boolean
  def canBeLong: Boolean

  def unary_-(): Number

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal

  def +(rhs: Number): Number
  def *(rhs: Number): Number
  def -(rhs: Number): Number
  def /(rhs: Number): Number
  def /~(rhs: Number): Number
  def %(rhs: Number): Number
  def /%(rhs: Number): (Number, Number)

  private[math] def r_-(lhs: Number): Number
  private[math] def r_/(lhs: Number): Number
  private[math] def r_/~(lhs: Number): Number
  private[math] def r_%(lhs: Number): Number
  private[math] def r_/%(lhs: Number): (Number, Number)

  def pow(rhs: Number): Number
  final def **(rhs: Number) = pow(rhs)

  def compare(rhs: Number): Int
  def min(rhs: Number): Number = if (this < rhs) this else rhs
  def max(rhs: Number): Number = if (this > rhs) this else rhs

  final def <(rhs: Number): Boolean = compare(rhs) < 0
  final def <=(rhs: Number): Boolean = compare(rhs) <= 0
  final def >(rhs: Number): Boolean = compare(rhs) > 0
  final def >=(rhs: Number): Boolean = compare(rhs) >= 0

  def &(rhs: Number): Number = sys.error("%s not an integer" format this)
  def |(rhs: Number): Number = sys.error("%s not an integer" format this)
  def ^(rhs: Number): Number = sys.error("%s not an integer" format this)
  def <<(rhs: Number): Number = sys.error("%s not an integer" format this)
  def >>(rhs: Number): Number = sys.error("%s not an integer" format this)

  def floor: Number
  def ceil: Number
  def round: Number
}


/**
 * Number with an underlying Long representation.
 */
private[math] case class IntNumber(n: SafeLong) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs = IntNumber(n.abs)
  def signum = n.signum

  def withinInt = Number.minInt <= n && n <= Number.maxInt
  def withinLong = Number.minLong <= n && n <= Number.maxLong
  def withinDouble = {
    val d = n.toBigDecimal
    Number.minDouble <= d && d <= Number.maxDouble
  }

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n.toBigDecimal

  def underlying = n.fold(x => new java.lang.Long(x), b => b)

  def isWhole = true
  def doubleValue = n.doubleValue
  def floatValue = n.floatValue
  def longValue = n.longValue
  def intValue = n.intValue

  def compare(rhs: Number) = rhs match {
    case IntNumber(m) => n.compare(m)
    case t => -t.compare(lhs)
  }

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => n == m
    case t: Number => t == this
    case t: BigDecimal => n.toBigDecimal == t
    case t: BigInt => n == t
    case t: Natural => n == t.toBigInt
    case t: SafeLong => n == t
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs: Number) = rhs match {
    case IntNumber(m) => IntNumber(n + m)
    case t => t + lhs
  }
  def *(rhs: Number) = rhs match {
    case IntNumber(m) => IntNumber(n * m)
    case t => t * lhs
  }
  def -(rhs: Number) = rhs match {
    case IntNumber(m) => IntNumber(n - m)
    case t => t r_- lhs
  }
  def /(rhs: Number) = rhs match {
    case IntNumber(m) => n.fold(
      x => m.fold(
        y => FloatNumber(x.toDouble / y.toDouble),
        y => DecimalNumber(BigDecimal(x) / BigDecimal(y))
      ),
      x => Number(BigDecimal(x) / m.toBigDecimal)
    )
    case t => t r_/ lhs
  }
  def /~(rhs: Number) = rhs match {
    case IntNumber(m) => IntNumber(n / m)
    case t => t r_/~ lhs
  }
  def %(rhs: Number) = rhs match {
    case IntNumber(m) => IntNumber(n % m)
    case t => t r_% lhs
  }
  def /%(rhs: Number) = rhs match {
    case IntNumber(m) => (IntNumber(n / m), IntNumber(n % m))
    case t => t r_/% lhs
  }

  private[math] def r_-(lhs: Number) = lhs match {
    case IntNumber(m) => IntNumber(m - n)
    case t => t - lhs
  }
  private[math] def r_/(lhs: Number) = lhs match {
    case IntNumber(m) => n.fold(
      x => m.fold(
        y => FloatNumber(y.toDouble / x.toDouble),
        y => DecimalNumber(BigDecimal(y) / BigDecimal(x))
      ),
      x => Number(m.toBigDecimal / BigDecimal(x))
    )
    case t => t / lhs
  }
  private[math] def r_/~(lhs: Number) = lhs match {
    case IntNumber(m) => IntNumber(m / n)
    case t => t /~ lhs
  }
  private[math] def r_%(lhs: Number) = lhs match {
    case IntNumber(m) => IntNumber(m % n)
    case t => t % lhs
  }
  private[math] def r_/%(lhs: Number) = lhs match {
    case IntNumber(m) => (IntNumber(m / n), IntNumber(m % n))
    case t => t /% lhs
  }

  def pow(rhs: Number) = rhs match {
    case _ if rhs.canBeInt => Number(n.pow(rhs.intValue))
    case FloatNumber(m) if (withinDouble) => Number(spire.math.pow(doubleValue, m))
    case _ => Number(spire.math.pow(lhs.toBigDecimal, rhs.toBigDecimal))
  }

  override def &(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n & x)
    case _ => sys.error("%s not an integer" format rhs)
  }
  override def |(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n | x)
    case _ => sys.error("%s not an integer" format rhs)
  }
  override def ^(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n ^ x)
    case _ => sys.error("%s not an integer" format rhs)
  }
  override def <<(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n << x.toInt)
    case _ => sys.error("%s not an integer" format rhs)
  }
  override def >>(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n >> x.toInt)
    case _ => sys.error("%s not an integer" format rhs)
  }

  def floor = this
  def ceil = this
  def round = this
}

private[math] case class FloatNumber(n: Double) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs = FloatNumber(Math.abs(n))
  def signum = Math.signum(n).toInt

  def withinInt = Int.MinValue.toDouble <= n && n <= Int.MaxValue.toDouble
  def withinLong = Long.MinValue.toDouble <= n && n <= Long.MaxValue.toDouble
  def withinDouble = Double.MinValue <= n && n <= Double.MaxValue

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def underlying = new java.lang.Double(n)
  def isWhole = (n % 1) == 0.0
  def doubleValue = n
  def floatValue = n.toFloat
  def longValue = n.toLong
  def intValue = n.toInt

  def toBigInt: BigInt = BigDecimal(n).toBigInt
  def toBigDecimal: BigDecimal = BigDecimal(n)

  def compare(rhs: Number) = rhs match {
    case IntNumber(m) => BigDecimal(n) compare m.toBigDecimal
    case FloatNumber(m) => n compare m
    case t => -t.compare(lhs)
  }

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => m == m.toDouble.toLong && m == n
    case FloatNumber(m) => n == m
    case t: Number => t == this
    case t: BigDecimal => BigDecimal(n) == t
    case t: BigInt => n == t
    case t: Natural => BigDecimal(t.toBigInt) == BigDecimal(n)
    case t: SafeLong => t == n
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n + x), x => Number(BigDecimal(x) + n))
    case FloatNumber(m) => Number(n + m)
    case t => t + lhs
  }

  def *(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n * x), x => Number(BigDecimal(n) * BigDecimal(x)))
    case FloatNumber(m) => Number(n * m)
    case t => t * lhs
  }

  def -(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n - x), x => Number(BigDecimal(n) + BigDecimal(x)))
    case FloatNumber(m) => Number(n - m)
    case t => t r_- lhs
  }
  private[math] def r_-(lhs: Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(x - n), x => Number(BigDecimal(x) - BigDecimal(n)))
    case FloatNumber(m) => Number(m - n)
    case t => t - lhs
  }

  def /(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n / x), x => Number(BigDecimal(n) / BigDecimal(x)))
    case FloatNumber(m) => Number(n / m)
    case t => t r_/ lhs
  }
  private[math] def r_/(lhs: Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(x / n), x => Number(BigDecimal(x) / BigDecimal(n)))
    case FloatNumber(m) => Number(m / n)
    case t => t / lhs
  }

  def /~(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(Math.floor(n / x)),
                                 x => Number(BigDecimal(n) quot BigDecimal(x)))
    case FloatNumber(m) => Number(Math.floor(n / m))
    case t => t r_/~ lhs
  }
  private[math] def r_/~(lhs: Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(Math.floor(x / n)),
                                 x => Number(BigDecimal(x) quot n))
    case FloatNumber(m) => Number(Math.floor(m / n))
    case t => t /~ lhs
  }
  
  def %(rhs: Number) = rhs match {
    case IntNumber(m) => m.fold(
      x => Number(n % x),
      x => Number(BigDecimal(n) % BigDecimal(x))
    )
    case FloatNumber(m) => Number(n % m)
    case t => t.r_%(lhs)
  }
  private[math] def r_%(lhs: Number) = lhs match {
    case IntNumber(m) => m.fold(
      x => Number(x % n),
      x => Number(BigDecimal(x) % n)
    )
    case FloatNumber(m) => Number(m % n)
    case t => t % lhs
  }

  def /%(rhs: Number) = rhs match {
    case IntNumber(m) => (FloatNumber(n / m.toDouble), FloatNumber(n % m.toDouble))
    case FloatNumber(m) => (FloatNumber(n / m), FloatNumber(n % m))
    case t => t r_/% lhs
  }
  private[math] def r_/%(lhs: Number) = lhs match {
    case IntNumber(m) => (FloatNumber(m.toDouble / n), FloatNumber(m.toDouble % n))
    case FloatNumber(m) => (FloatNumber(m / n), FloatNumber(m % n))
    case t => t /% lhs
  }

  def pow(rhs: Number) = rhs match {
    case FloatNumber(m) => Number(spire.math.pow(n, m))
    case _ if rhs.withinDouble => Number(spire.math.pow(n, rhs.doubleValue));
    case _ => Number(spire.math.pow(BigDecimal(n), rhs.toBigDecimal))
  }

  def floor = Number(Math.floor(n))
  def ceil = Number(Math.ceil(n))
  def round = Number(Math.round(n))
}


private[math] case class DecimalNumber(n: BigDecimal) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs = DecimalNumber(n.abs)
  def signum = n.signum

  def withinInt = BigDecimal(Int.MinValue) <= n && n <= BigDecimal(Int.MaxValue)
  def withinLong = BigDecimal(Long.MinValue) <= n && n <= BigDecimal(Long.MaxValue)
  def withinDouble = BigDecimal(Double.MinValue) <= n && n <= BigDecimal(Double.MaxValue)

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def underlying = n
  def isWhole = n % 1 == 0
  def doubleValue = n.toDouble
  def floatValue = n.toFloat
  def longValue = n.toLong
  def intValue = n.toInt

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n

  def compare(rhs: Number) = n compare rhs.toBigDecimal

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => n == m.toBigDecimal
    case FloatNumber(m) => n == m
    case DecimalNumber(m) => n == m
    case t: BigDecimal => n == t
    case t: BigInt => n == t
    case t: Natural => n == t.toBigInt
    case t: SafeLong => n == t.toBigInt
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs: Number) = Number(n + rhs.toBigDecimal)
  def *(rhs: Number) = Number(n + rhs.toBigDecimal)
  def -(rhs: Number) = Number(n - rhs.toBigDecimal)
  def /(rhs: Number) = Number(n / rhs.toBigDecimal)
  def /~(rhs: Number) = Number(n quot rhs.toBigDecimal)
  def %(rhs: Number) = Number(n % rhs.toBigDecimal)

  def r_-(lhs: Number) = Number(lhs.toBigDecimal - n)
  def r_/(lhs: Number) = Number(lhs.toBigDecimal / n)
  def r_/~(lhs: Number) = Number(lhs.toBigDecimal quot n)
  def r_%(lhs: Number) = Number(lhs.toBigDecimal % n)

  private def tuplize(t: (BigDecimal, BigDecimal)) = (DecimalNumber(t._1), DecimalNumber(t._2))

  def /%(rhs: Number) = {
    val t = n /% rhs.toBigDecimal
    (Number(t._1), Number(t._2))
  }

  def r_/%(lhs: Number) = {
    val t = lhs.toBigDecimal /% n
    (Number(t._1), Number(t._2))
  }

  def pow(rhs: Number) = if (rhs.canBeInt) {
    Number(n.pow(rhs.intValue))
  } else {
    Number(spire.math.pow(n, rhs.toBigDecimal))
  }

  import spire.algebra.Field

  def floor = DecimalNumber(Field[BigDecimal].floor(n))
  def ceil = DecimalNumber(Field[BigDecimal].ceil(n))
  def round = DecimalNumber(Field[BigDecimal].round(n))
}

trait NumberInstances {
  implicit object NumberAlgebra extends NumberIsField with NumberIsNRoot
  implicit object NumberIsReal extends NumberIsReal
}

trait NumberIsRing extends Ring[Number] {
  override def minus(a:Number, b:Number): Number = a - b
  def negate(a:Number): Number = -a
  val one: Number = Number.one
  def plus(a:Number, b:Number): Number = a + b
  override def pow(a:Number, b:Int): Number = a.pow(Number(b))
  override def times(a:Number, b:Number): Number = a * b
  val zero: Number = Number.zero
  
  override def fromInt(n: Int): Number = Number(n)
}

trait NumberIsEuclideanRing extends EuclideanRing[Number] with NumberIsRing {
  def quot(a:Number, b:Number) = a / b
  def mod(a:Number, b:Number) = a % b
  override def quotmod(a:Number, b:Number) = a /% b
  def gcd(a: Number, b: Number): Number = euclid(a, b)(Eq[Number])
}

trait NumberIsField extends Field[Number] with NumberIsEuclideanRing {
  def div(a:Number, b:Number) = a / b
  def ceil(a:Number): Number = a.ceil
  def floor(a:Number): Number = a.floor
  def round(a:Number): Number = a.round
  def isWhole(a:Number) = a.isWhole
}

trait NumberIsNRoot extends NRoot[Number] {
  def nroot(a: Number, k: Int): Number = a.pow(Number(k))
  override def sqrt(a: Number): Number = a.pow(Number(0.5))
  def log(a: Number) = Number(Math.log(a.toDouble))
  def fpow(a: Number, b: Number) = a.pow(b)
}

trait NumberEq extends Eq[Number] {
  def eqv(x: Number, y: Number) = x == y
  override def neqv(x: Number, y: Number) = x != y
}

trait NumberOrder extends Order[Number] with NumberEq {
  override def gt(x: Number, y: Number) = x > y
  override def gteqv(x: Number, y: Number) = x >= y
  override def lt(x: Number, y: Number) = x < y
  override def lteqv(x: Number, y: Number) = x <= y
  def compare(x: Number, y: Number) = x.compare(y)
}

trait NumberIsSigned extends Signed[Number] {
  def signum(a: Number): Int = a.signum
  def abs(a: Number): Number = a.abs
}

trait NumberIsReal extends IsReal[Number] with NumberOrder with NumberIsSigned {
  def toDouble(x: Number): Double = x.toDouble
}
