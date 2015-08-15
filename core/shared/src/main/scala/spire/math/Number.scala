package spire.math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.lang.Math

import spire.algebra.{Eq, EuclideanRing, Field, IsReal, IsRational, NRoot, Order, Ring, Signed, Trig}
import spire.std.bigDecimal._
import spire.syntax.isReal._
import spire.syntax.nroot._

// TODO: implement toNumber and fromNumber in ConvertableTo/From.
// TODO: pow() is hairy; should support more cases and generate better errors
// TODO: decide what should be public/private

/**
 * Convenient apply and implicits for Numbers
 */
object Number extends NumberInstances {

  final val zero: Number = Number(0)
  final val one: Number = Number(1)

  implicit def apply(n: Int): Number = IntNumber(SafeLong(n))
  implicit def apply(n: Long): Number = IntNumber(SafeLong(n))
  implicit def apply(n: BigInt): Number = IntNumber(SafeLong(n))
  implicit def apply(n: SafeLong): Number = IntNumber(n)
  implicit def apply(n: BigDecimal): Number = DecimalNumber(n)
  implicit def apply(n: Rational): Number = RationalNumber(n)
  implicit def apply(n: Natural): Number = IntNumber(n.toBigInt)

  implicit def apply(n: Float): Number =
    if (java.lang.Float.isNaN(n) || java.lang.Float.isInfinite(n))
      throw new IllegalArgumentException(n.toString)
    else
      FloatNumber(n)

  implicit def apply(n: Double): Number =
    if (java.lang.Double.isNaN(n) || java.lang.Double.isInfinite(n))
      throw new IllegalArgumentException(n.toString)
    else
      FloatNumber(n)

  def apply(s: String): Number =
    try {
      Number(SafeLong(s))
    } catch {
      case _: Exception => Number(BigDecimal(s))
    }

  private[math] val minInt = SafeLong(Int.MinValue)
  private[math] val maxInt = SafeLong(Int.MaxValue)

  private[math] val minLong = SafeLong(Long.MinValue)
  private[math] val maxLong = SafeLong(Long.MaxValue)

  private[math] val minDouble = BigDecimal(Double.MinValue)
  private[math] val maxDouble = BigDecimal(Double.MaxValue)
}

sealed trait Number extends ScalaNumericConversions with Serializable {
  def abs: Number
  def signum: Int

  def withinInt: Boolean
  def withinLong: Boolean
  def withinDouble: Boolean

  def canBeInt: Boolean
  def canBeLong: Boolean
  def isExact: Boolean

  def unary_-(): Number

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal
  def toRational: Rational

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
  final def **(rhs: Number): Number = pow(rhs)

  def ===(rhs: Number): Boolean
  def =!=(rhs: Number): Boolean = !(this === rhs)

  def compare(rhs: Number): Int
  def min(rhs: Number): Number = if (this < rhs) this else rhs
  def max(rhs: Number): Number = if (this > rhs) this else rhs

  final def <(rhs: Number): Boolean = compare(rhs) < 0
  final def <=(rhs: Number): Boolean = compare(rhs) <= 0
  final def >(rhs: Number): Boolean = compare(rhs) > 0
  final def >=(rhs: Number): Boolean = compare(rhs) >= 0

  def &(rhs: Number): Number = throw new UnsupportedOperationException("%s not an integer" format this)
  def |(rhs: Number): Number = throw new UnsupportedOperationException("%s not an integer" format this)
  def ^(rhs: Number): Number = throw new UnsupportedOperationException("%s not an integer" format this)
  def <<(rhs: Number): Number = throw new UnsupportedOperationException("%s not an integer" format this)
  def >>(rhs: Number): Number = throw new UnsupportedOperationException("%s not an integer" format this)

  def floor: Number
  def ceil: Number
  def round: Number
}


/**
 * Number with an underlying Long representation.
 */
private[math] case class IntNumber(n: SafeLong) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs: IntNumber = IntNumber(n.abs)
  def signum: Int = n.signum

  def withinInt: Boolean = Number.minInt <= n && n <= Number.maxInt
  def withinLong: Boolean = Number.minLong <= n && n <= Number.maxLong
  def withinDouble: Boolean = {
    val d = n.toBigDecimal
    Number.minDouble <= d && d <= Number.maxDouble
  }

  def canBeInt: Boolean = isWhole && withinInt
  def canBeLong: Boolean = isWhole && withinLong
  def isExact: Boolean = true

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n.toBigDecimal
  def toRational: Rational = Rational(n)

  def underlying: java.lang.Object = n.underlying

  def isWhole: Boolean = true
  def doubleValue: Double = n.doubleValue
  def floatValue: Float = n.floatValue
  def longValue: Long = n.longValue
  def intValue: Int = n.intValue

  def compare(rhs: Number): Int = rhs match {
    case IntNumber(m) => n.compare(m)
    case t => -t.compare(lhs)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Number => this === that
      case that => n == that
    }

  def ===(that: Number): Boolean =
    that match {
      case IntNumber(n2) => n == n2
      case that => that === this
    }

  def unary_- : Number = Number(-n)

  def +(rhs: Number): Number = rhs match {
    case IntNumber(m) => IntNumber(n + m)
    case t => t + lhs
  }
  def *(rhs: Number): Number = rhs match {
    case IntNumber(m) => IntNumber(n * m)
    case t => t * lhs
  }
  def -(rhs: Number): Number = rhs match {
    case IntNumber(m) => IntNumber(n - m)
    case t => t r_- lhs
  }
  def /(rhs: Number): Number = rhs match {
    case IntNumber(m) => n match {
      case SafeLongLong(x) => m match {
        case SafeLongLong(y) => Number(x.toDouble / y.toDouble)
        case SafeLongBigInteger(y) => DecimalNumber(BigDecimal(x) / BigDecimal(y))
      }
      case SafeLongBigInteger(x) => Number(BigDecimal(x) / m.toBigDecimal)
    }
    case t => t r_/ lhs
  }
  def /~(rhs: Number): Number = rhs match {
    case IntNumber(m) => IntNumber(n / m)
    case t => t r_/~ lhs
  }
  def %(rhs: Number): Number = rhs match {
    case IntNumber(m) => IntNumber(n % m)
    case t => t r_% lhs
  }
  def /%(rhs: Number): (Number, Number) = rhs match {
    case IntNumber(m) => (IntNumber(n / m), IntNumber(n % m))
    case t => t r_/% lhs
  }

  private[math] def r_-(lhs: Number): Number = lhs match {
    case IntNumber(m) => IntNumber(m - n)
    case t => t - lhs
  }
  private[math] def r_/(lhs: Number): Number = lhs match {
    case IntNumber(m) => n match {
      case SafeLongLong(x) => m match {
        case SafeLongLong(y) => Number(y.toDouble / x.toDouble)
        case SafeLongBigInteger(y) => DecimalNumber(BigDecimal(y) / BigDecimal(x))
      }
      case SafeLongBigInteger(x) => Number(m.toBigDecimal / BigDecimal(x))
    }
    case t => t / lhs
  }
  private[math] def r_/~(lhs: Number): Number = lhs match {
    case IntNumber(m) => IntNumber(m / n)
    case t => t /~ lhs
  }
  private[math] def r_%(lhs: Number): Number = lhs match {
    case IntNumber(m) => IntNumber(m % n)
    case t => t % lhs
  }
  private[math] def r_/%(lhs: Number): (Number, Number) = lhs match {
    case IntNumber(m) => (IntNumber(m / n), IntNumber(m % n))
    case t => t /% lhs
  }

  def pow(rhs: Number): Number = rhs match {
    case _ if rhs.canBeInt => Number(n.pow(rhs.intValue))
    case FloatNumber(m) if (withinDouble) => Number(spire.math.pow(doubleValue, m))
    case _ => Number(spire.math.pow(lhs.toBigDecimal, rhs.toBigDecimal))
  }

  override def &(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n & x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def |(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n | x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def ^(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n ^ x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def <<(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n << x.toInt)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def >>(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n >> x.toInt)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }

  def sqrt: Number =
    if (withinDouble)
      Number(Math.sqrt(n.toDouble))
    else
      Number(n.toBigDecimal.sqrt)

  def nroot(k: Int): Number =
    if (withinDouble)
      Number(Math.pow(n.toDouble, 1.0 / k))
    else
      Number(n.toBigDecimal.nroot(k))

  def floor: Number = this
  def ceil: Number = this
  def round: Number = this
}

private[math] case class FloatNumber(n: Double) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs: FloatNumber = FloatNumber(Math.abs(n))
  def signum: Int = Math.signum(n).toInt

  def withinInt: Boolean = Int.MinValue.toDouble <= n && n <= Int.MaxValue.toDouble
  def withinLong: Boolean = Long.MinValue.toDouble <= n && n <= Long.MaxValue.toDouble
  def withinDouble: Boolean = Double.MinValue <= n && n <= Double.MaxValue

  def canBeInt: Boolean = isWhole && withinInt
  def canBeLong: Boolean = isWhole && withinLong
  def isExact: Boolean = false

  def underlying: java.lang.Double = new java.lang.Double(n)
  def isWhole: Boolean = (n % 1) == 0.0
  def doubleValue: Double = n
  def floatValue: Float = n.toFloat
  def longValue: Long = n.toLong
  def intValue: Int = n.toInt

  def toBigInt: BigInt = BigDecimal(n).toBigInt
  def toBigDecimal: BigDecimal = BigDecimal(n)
  def toRational: Rational = Rational(n)

  def compare(rhs: Number): Int = rhs match {
    case IntNumber(m) => BigDecimal(n) compare m.toBigDecimal
    case FloatNumber(m) => n compare m
    case t => -t.compare(lhs)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Number => this === that
      case that => n == that
    }

  def ===(that: Number): Boolean =
    that match {
      case FloatNumber(n2) => n == n2
      case IntNumber(m) => m == m.toDouble.toLong && m == n
      case _ => that == this
    }

  def unary_- : Number = Number(-n)

  def +(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(n + x)
      case SafeLongBigInteger(x) => Number(BigDecimal(x) + n)
    }
    case FloatNumber(m) => Number(n + m)
    case t => t + lhs
  }

  def *(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(n * x)
      case SafeLongBigInteger(x) => Number(BigDecimal(n) * BigDecimal(x))
    }
    case FloatNumber(m) => Number(n * m)
    case t => t * lhs
  }

  def -(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(n - x)
      case SafeLongBigInteger(x) => Number(BigDecimal(n) + BigDecimal(x))
    }
    case FloatNumber(m) => Number(n - m)
    case t => t r_- lhs
  }
  private[math] def r_-(lhs: Number): Number = lhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(x - n)
      case SafeLongBigInteger(x) => Number(BigDecimal(x) - BigDecimal(n))
    }
    case FloatNumber(m) => Number(m - n)
    case t => t - lhs
  }

  def /(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(n / x)
      case SafeLongBigInteger(x) => Number(BigDecimal(n) / BigDecimal(x))
    }
    case FloatNumber(m) => Number(n / m)
    case t => t r_/ lhs
  }
  private[math] def r_/(lhs: Number): Number = lhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(x / n)
      case SafeLongBigInteger(x) => Number(BigDecimal(x) / BigDecimal(n))
    }
    case FloatNumber(m) => Number(m / n)
    case t => t / lhs
  }

  def /~(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(Math.floor(n / x))
      case SafeLongBigInteger(x) => Number(BigDecimal(n) quot BigDecimal(x))
    }
    case FloatNumber(m) => Number(Math.floor(n / m))
    case t => t r_/~ lhs
  }
  private[math] def r_/~(lhs: Number): Number = lhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(Math.floor(x / n))
      case SafeLongBigInteger(x) => Number(BigDecimal(x) quot n)
    }
    case FloatNumber(m) => Number(Math.floor(m / n))
    case t => t /~ lhs
  }

  def %(rhs: Number): Number = rhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(n % x)
      case SafeLongBigInteger(x) => Number(BigDecimal(n) % BigDecimal(x))
    }
    case FloatNumber(m) => Number(n % m)
    case t => t.r_%(lhs)
  }
  private[math] def r_%(lhs: Number): Number = lhs match {
    case IntNumber(m) => m match {
      case SafeLongLong(x) => Number(x % n)
      case SafeLongBigInteger(x) => Number(BigDecimal(x) % n)
    }
    case FloatNumber(m) => Number(m % n)
    case t => t % lhs
  }

  def /%(rhs: Number): (Number, Number) = rhs match {
    case IntNumber(m) => (Number(n / m.toDouble), Number(n % m.toDouble))
    case FloatNumber(m) => (Number(n / m), Number(n % m))
    case t => t r_/% lhs
  }
  private[math] def r_/%(lhs: Number): (Number, Number) = lhs match {
    case IntNumber(m) => (Number(m.toDouble / n), Number(m.toDouble % n))
    case FloatNumber(m) => (Number(m / n), Number(m % n))
    case t => t /% lhs
  }

  def pow(rhs: Number): Number = rhs match {
    case FloatNumber(m) => Number(spire.math.pow(n, m))
    case _ if rhs.withinDouble => Number(spire.math.pow(n, rhs.doubleValue));
    case _ => Number(spire.math.pow(BigDecimal(n), rhs.toBigDecimal))
  }

  def sqrt: Number = Number(Math.sqrt(n))
  def nroot(k: Int): Number = Number(Math.pow(n, 1.0 / k))

  def floor: Number = Number(Math.floor(n))
  def ceil: Number = Number(Math.ceil(n))
  def round: Number = Number(Math.round(n))
}


private[math] case class DecimalNumber(n: BigDecimal) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs: Number = DecimalNumber(n.abs)
  def signum: Int = n.signum

  def withinInt: Boolean = BigDecimal(Int.MinValue) <= n && n <= BigDecimal(Int.MaxValue)
  def withinLong: Boolean = BigDecimal(Long.MinValue) <= n && n <= BigDecimal(Long.MaxValue)
  def withinDouble: Boolean = BigDecimal(Double.MinValue) <= n && n <= BigDecimal(Double.MaxValue)

  def canBeInt: Boolean = isWhole && withinInt
  def canBeLong: Boolean = isWhole && withinLong
  def isExact: Boolean = false

  def underlying: BigDecimal = n
  def isWhole: Boolean = n % 1 == 0
  def doubleValue: Double = n.toDouble
  def floatValue: Float = n.toFloat
  def longValue: Long = n.toLong
  def intValue: Int = n.toInt

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n
  def toRational: Rational = Rational(n)

  def compare(rhs: Number): Int = n compare rhs.toBigDecimal

  override def equals(that: Any): Boolean =
    that match {
      case that: Number => this === that
      case that => that == n
    }

  def ===(that: Number): Boolean =
    that match {
      case DecimalNumber(n2) => n == n2
      case IntNumber(m) => n == m.toBigDecimal
      case FloatNumber(m) => n == m
      case RationalNumber(m) => m == n
    }

  def unary_- : Number = Number(-n)

  def +(rhs: Number): Number = Number(n + rhs.toBigDecimal)
  def *(rhs: Number): Number = Number(n * rhs.toBigDecimal)
  def -(rhs: Number): Number = Number(n - rhs.toBigDecimal)
  def /(rhs: Number): Number = Number(n / rhs.toBigDecimal)
  def /~(rhs: Number): Number = Number(n quot rhs.toBigDecimal)
  def %(rhs: Number): Number = Number(n % rhs.toBigDecimal)

  def r_-(lhs: Number): Number = Number(lhs.toBigDecimal - n)
  def r_/(lhs: Number): Number = Number(lhs.toBigDecimal / n)
  def r_/~(lhs: Number): Number = Number(lhs.toBigDecimal quot n)
  def r_%(lhs: Number): Number = Number(lhs.toBigDecimal % n)

  private def tuplize(t: (BigDecimal, BigDecimal)) = (DecimalNumber(t._1), DecimalNumber(t._2))

  def /%(rhs: Number): (Number, Number) = {
    val t = n /% rhs.toBigDecimal
    (Number(t._1), Number(t._2))
  }

  def r_/%(lhs: Number): (Number, Number) = {
    val t = lhs.toBigDecimal /% n
    (Number(t._1), Number(t._2))
  }

  def pow(rhs: Number): Number = if (rhs.canBeInt) {
    Number(n.pow(rhs.intValue))
  } else {
    Number(spire.math.pow(n, rhs.toBigDecimal))
  }

  def sqrt: Number = Number(n.sqrt)
  def nroot(k: Int): Number = Number(n.nroot(k))

  def floor: Number = Number(n.floor)
  def ceil: Number = Number(n.ceil)
  def round: Number = Number(n.round())
}

private[math] case class RationalNumber(n: Rational) extends Number { lhs =>

  override def toString(): String = n.toString

  def abs: Number = RationalNumber(n.abs)
  def signum: Int = n.signum

  def withinInt: Boolean = Rational(Int.MinValue) <= n && n <= Rational(Int.MaxValue)
  def withinLong: Boolean = Rational(Long.MinValue) <= n && n <= Rational(Long.MaxValue)
  def withinDouble: Boolean = Rational(Double.MinValue) <= n && n <= Rational(Double.MaxValue)

  def canBeInt: Boolean = isWhole && withinInt
  def canBeLong: Boolean = isWhole && withinLong
  def isExact: Boolean = true

  def underlying: Rational = n
  def isWhole: Boolean = n.isWhole
  def doubleValue: Double = n.toDouble
  def floatValue: Float = n.toFloat
  def longValue: Long = n.toLong
  def intValue: Int = n.toInt

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n.toBigDecimal(BigDecimal.defaultMathContext)
  def toRational: Rational = n

  def compare(rhs: Number): Int = n compare rhs.toRational

  override def equals(that: Any): Boolean =
    that match {
      case that: Number => this === that
      case that => n == that
    }

  def ===(that: Number): Boolean =
    that match {
      case RationalNumber(n2) => n == n2
      case IntNumber(m) => n == m.toBigDecimal
      case FloatNumber(m) => n == m
      case DecimalNumber(m) => n == m
    }

  def unary_- : Number = Number(-n)

  def +(rhs: Number): Number = Number(n + rhs.toRational)
  def *(rhs: Number): Number = Number(n * rhs.toRational)
  def -(rhs: Number): Number = Number(n - rhs.toRational)
  def /(rhs: Number): Number = Number(n / rhs.toRational)
  def /~(rhs: Number): Number = Number(n /~ rhs.toRational)
  def %(rhs: Number): Number = Number(n % rhs.toRational)

  def r_-(lhs: Number): Number = Number(lhs.toRational - n)
  def r_/(lhs: Number): Number = Number(lhs.toRational / n)
  def r_/~(lhs: Number): Number = Number(lhs.toRational /~ n)
  def r_%(lhs: Number): Number = Number(lhs.toRational % n)

  private def tuplize(t: (Rational, Rational)) = (RationalNumber(t._1), RationalNumber(t._2))

  def /%(rhs: Number): (Number, Number) = {
    val t = n /% rhs.toRational
    (Number(t._1), Number(t._2))
  }

  def r_/%(lhs: Number): (Number, Number) = {
    val t = lhs.toRational /% n
    (Number(t._1), Number(t._2))
  }

  def pow(rhs: Number): Number = if (rhs.canBeInt) {
    Number(n.pow(rhs.intValue))
  } else {
    // FIXME: we should actually try to return values with a meaningful approximation context
    Number(spire.math.pow(n.toDouble, rhs.toDouble))
  }

  import spire.algebra.Field

  def floor: Number = RationalNumber(IsReal[Rational].floor(n))
  def ceil: Number = RationalNumber(IsReal[Rational].ceil(n))
  def round: Number = RationalNumber(IsReal[Rational].round(n))
}

trait NumberInstances {
  implicit final val NumberAlgebra = new NumberAlgebra
}

private[math] trait NumberIsRing extends Ring[Number] {
  override def minus(a:Number, b:Number): Number = a - b
  def negate(a:Number): Number = -a
  def one: Number = Number.one
  def plus(a:Number, b:Number): Number = a + b
  override def pow(a:Number, b:Int): Number = a.pow(Number(b))
  override def times(a:Number, b:Number): Number = a * b
  def zero: Number = Number.zero

  override def fromInt(n: Int): Number = Number(n)
}

private[math] trait NumberIsEuclideanRing extends EuclideanRing[Number] with NumberIsRing {
  def quot(a:Number, b:Number): Number = a / b
  def mod(a:Number, b:Number): Number = a % b
  override def quotmod(a:Number, b:Number): (Number, Number) = a /% b
  def gcd(a: Number, b: Number): Number = euclid(a, b)(Eq[Number])
}

private[math] trait NumberIsField extends Field[Number] with NumberIsEuclideanRing {
  def div(a:Number, b:Number): Number = a / b
  override def fromDouble(a: Double): Number = Number(a)
}

private[math] trait NumberIsNRoot extends NRoot[Number] {
  def nroot(a: Number, k: Int): Number = a.pow(Number(k))
  override def sqrt(a: Number): Number = a.pow(Number(0.5))
  def fpow(a: Number, b: Number): Number = a.pow(b)
}

private[math] trait NumberIsTrig extends Trig[Number] {
  def e: Number = Number(Math.E)
  def pi: Number = Number(Math.PI)

  def exp(a: Number): Number = Math.exp(a.toDouble)
  def expm1(a: Number): Number = Math.expm1(a.toDouble)
  def log(a: Number): Number = Number(Math.log(a.toDouble))
  def log1p(a: Number): Number = Number(Math.log1p(a.toDouble))

  def sin(a: Number): Number = Math.sin(a.toDouble)
  def cos(a: Number): Number = Math.cos(a.toDouble)
  def tan(a: Number): Number = Math.tan(a.toDouble)

  def asin(a: Number): Number = Math.asin(a.toDouble)
  def acos(a: Number): Number = Math.acos(a.toDouble)
  def atan(a: Number): Number = Math.atan(a.toDouble)
  def atan2(y: Number, x: Number): Number = Math.atan2(y.toDouble, x.toDouble)

  def sinh(x: Number): Number = Math.sinh(x.toDouble)
  def cosh(x: Number): Number = Math.cosh(x.toDouble)
  def tanh(x: Number): Number = Math.tanh(x.toDouble)

  def toRadians(a: Number): Number = (a * 2 * pi) / 360
  def toDegrees(a: Number): Number = (a * 360) / (2 * pi)
}

private[math] trait NumberOrder extends Order[Number] {
  override def eqv(x: Number, y: Number): Boolean = x == y
  override def neqv(x: Number, y: Number): Boolean = x != y
  override def gt(x: Number, y: Number): Boolean = x > y
  override def gteqv(x: Number, y: Number): Boolean = x >= y
  override def lt(x: Number, y: Number): Boolean = x < y
  override def lteqv(x: Number, y: Number): Boolean = x <= y
  def compare(x: Number, y: Number): Int = x.compare(y)
}

private[math] trait NumberIsSigned extends Signed[Number] {
  def signum(a: Number): Int = a.signum
  def abs(a: Number): Number = a.abs
}

private[math] trait NumberIsReal extends IsRational[Number] with NumberOrder with NumberIsSigned {
  def toDouble(x: Number): Double = x.toDouble
  def ceil(a:Number): Number = a.ceil
  def floor(a:Number): Number = a.floor
  def round(a:Number): Number = a.round
  def isWhole(a:Number): Boolean = a.isWhole
  def toRational(a:Number): Rational = a.toRational
}

@SerialVersionUID(0L)
class NumberAlgebra extends NumberIsField with NumberIsNRoot with NumberIsTrig with NumberIsReal with Serializable
