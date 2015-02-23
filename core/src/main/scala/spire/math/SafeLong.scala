package spire.math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import scala.annotation.tailrec

import spire.macros.Checked

import spire.algebra.{EuclideanRing, IsIntegral, NRoot, Order, Ring, Signed}
import spire.std.long._
import spire.std.bigInt._

/**
 * Provides a type to do safe long arithmetic. This type will never overflow,
 * but rather convert the underlying long to a BigInt as need and back down
 * to a Long when possible.
 */
sealed trait SafeLong extends ScalaNumber with ScalaNumericConversions with Ordered[SafeLong] { lhs =>

  def isZero: Boolean

  def signum: Int

  final def +(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs + n
      case SafeLongBigInt(n) => lhs + n
    }

  final def -(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs - n
      case SafeLongBigInt(n) => lhs - n
    }

  final def *(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs * n
      case SafeLongBigInt(n) => lhs * n
    }

  final def /(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs / n
      case SafeLongBigInt(n) => lhs / n
    }

  final def %(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs % n
      case SafeLongBigInt(n) => lhs % n
    }

  final def /~(rhs: SafeLong): SafeLong =
    lhs / rhs

  final def /%(rhs: SafeLong): (SafeLong, SafeLong) =
    rhs match {
      case SafeLongLong(n) => lhs /% n
      case SafeLongBigInt(n) => lhs /% n
    }

  final def &(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs & n
      case SafeLongBigInt(n) => lhs & n
    }

  final def |(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs | n
      case SafeLongBigInt(n) => lhs | n
    }

  final def ^(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs ^ n
      case SafeLongBigInt(n) => lhs ^ n
    }

  def +(rhs: Long): SafeLong
  def -(rhs: Long): SafeLong
  def *(rhs: Long): SafeLong
  def /(rhs: Long): SafeLong
  def %(rhs: Long): SafeLong
  def /%(rhs: Long): (SafeLong, SafeLong)
  def &(rhs: Long): SafeLong
  def |(rhs: Long): SafeLong
  def ^(rhs: Long): SafeLong

  def +(rhs: BigInt): SafeLong
  def -(rhs: BigInt): SafeLong
  def *(rhs: BigInt): SafeLong
  def /(rhs: BigInt): SafeLong
  def %(rhs: BigInt): SafeLong
  def /%(rhs: BigInt): (SafeLong, SafeLong)
  def &(rhs: BigInt): SafeLong
  def |(rhs: BigInt): SafeLong
  def ^(rhs: BigInt): SafeLong

  final def min(that: SafeLong): SafeLong =
    if (this < that) this else that

  final def max(that: SafeLong): SafeLong =
    if (this > that) this else that

  def <<(n: Int): SafeLong
  def >>(n: Int): SafeLong

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^exponent doesn't fit in a Long, the result will overflow (unlike
   * scala.math.pow which will return +/- Infinity).
   */
  final def **(k: Int):SafeLong = pow(k)

  final def pow(k: Int): SafeLong = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    @tailrec def loop(total: SafeLong, base: SafeLong, exp: Int): SafeLong = {
      if (exp == 0) return total
      else if ((exp & 1) == 1) loop(total * base, base * base, exp >> 1)
      else loop(total, base * base, exp >> 1)
    }

    loop(SafeLong.one, this, k)
  }

  final def modPow(k: Int, mod: SafeLong): SafeLong = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    @tailrec def loop(total: SafeLong, base: SafeLong, k: Int, mod: SafeLong): SafeLong = {
      if (k == 0) return total
      else if ((k & 1) == 1) loop((total * base) % mod, (base * base) % mod, k >> 1, mod)
      else loop(total, (base * base) % mod, k >> 1, mod)
    }

    loop(SafeLong.one % mod, this, k, mod)
  }

  def abs: SafeLong

  def gcd(that: SafeLong): SafeLong

  def unary_-(): SafeLong

  def isValidLong: Boolean
  def getLong: Option[Long]

  override def toByte: Byte = toLong.toByte
  override def toShort: Short = toLong.toShort
  override def toInt: Int = toLong.toInt
  def toBigInt: BigInt
  def toBigDecimal: BigDecimal

  override def toString: String =
    this match {
      case SafeLongLong(n) => n.toString
      case SafeLongBigInt(n) => n.toString
    }

  final def isWhole: Boolean = true

  final def isProbablePrime(c: Int): Boolean =
    toBigInt.isProbablePrime(c)

  def bitLength: Int
}


object SafeLong extends SafeLongInstances {

  final val minusOne: SafeLong = SafeLongLong(-1L)
  final val zero: SafeLong = SafeLongLong(0L)
  final val one: SafeLong = SafeLongLong(1L)
  final val two: SafeLong = SafeLongLong(2L)
  final val three: SafeLong = SafeLongLong(3L)
  final val ten: SafeLong = SafeLongLong(10L)

  private[spire] final val big64: BigInt = BigInt(1) << 63
  private[spire] final val safe64: SafeLong = SafeLong(big64)

  implicit def apply(x: Long): SafeLong = SafeLongLong(x)

  implicit def apply(x: BigInt): SafeLong =
    if (x.isValidLong) SafeLongLong(x.toLong) else SafeLongBigInt(x)

  def apply(s: String): SafeLong =
    try {
      SafeLong(java.lang.Long.parseLong(s))
    } catch {
      case _: Exception => SafeLong(BigInt(s))
    }

  def longGcd(x: Long, y: Long): SafeLong = {
    def absWrap(x: Long): SafeLong =
      if (x >= 0) SafeLong(x)
      else if (x == Long.MinValue) SafeLong.safe64
      else SafeLong(-x)

    if (x == 0) absWrap(y)
    else if (y == 0) absWrap(x)
    else if (x == Long.MinValue) {
      if (y == Long.MinValue) SafeLong.safe64
      else spire.math.gcd(y, x % y)
    } else if (y == Long.MinValue) SafeLongLong(spire.math.gcd(x, y % x))
    else SafeLongLong(spire.math.gcd(x, y % x))
  }

  def mixedGcd(x: Long, y: BigInt): SafeLong =
    if (y.signum == 0) {
      if (x >= 0) SafeLongLong(x)
      else if (x == Long.MinValue) SafeLong.safe64
      else SafeLongLong(-x)
    } else if (x == 0L) {
      SafeLong(y.abs)
    } else if (x == Long.MinValue) {
      SafeLong(SafeLong.big64 gcd y)
    } else {
      SafeLongLong(spire.math.gcd(x, (y % x).toLong))
    }
}


case class SafeLongLong private[math] (x: Long) extends SafeLong {

  def isZero: Boolean = x == 0
  def signum: Int = java.lang.Long.signum(x)

  def +(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x + y))(SafeLongBigInt(BigInt(x) + y))

  def -(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x - y))(SafeLongBigInt(BigInt(x) - y))

  def *(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x * y))(SafeLongBigInt(BigInt(x) * y))

  def /(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x / y))(SafeLongBigInt(Long.MaxValue) + 1)

  def %(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x % y))(SafeLong.zero)

  def /%(y: Long) =
    if (x == Long.MinValue && y == -1L)
      (SafeLongBigInt(Long.MaxValue) + 1, SafeLong.zero)
    else
      (SafeLongLong(x / y), SafeLongLong(x % y))

  def &(y: Long): SafeLong = SafeLongLong(x & y)
  def |(y: Long): SafeLong = SafeLongLong(x | y)
  def ^(y: Long): SafeLong = SafeLongLong(x ^ y)

  def +(y: BigInt): SafeLong =
    if (y.bitLength <= 63) this + y.toLong
    else SafeLong(y + x)

  def -(y: BigInt): SafeLong =
    if (y.bitLength <= 63) this - y.toLong
    else SafeLong(BigInt(x) - y)

  def *(y: BigInt): SafeLong =
    if (y.bitLength <= 63) this * y.toLong
    else SafeLong(y * x)

  def /(y: BigInt): SafeLong =
    if (y.bitLength <= 63) this / y.toLong
    else if (x == Long.MinValue && -y == x) SafeLong.minusOne
    else SafeLong.zero

  def %(y: BigInt): SafeLong =
    if (y.bitLength <= 63) this % y.toLong
    else if (x == Long.MinValue && -y == x) SafeLong.zero
    else this

  def /%(y: BigInt) =
    if (y.bitLength <= 63) this /% y.toLong
    else if (x == Long.MinValue && -y == x) (SafeLong.minusOne, SafeLong.zero)
    else (SafeLong.zero, this)

  def &(y: BigInt): SafeLong = SafeLong(y & x)
  def |(y: BigInt): SafeLong = SafeLong(y | x)
  def ^(y: BigInt): SafeLong = SafeLong(y ^ x)

  def unary_-(): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(-x))(SafeLongBigInt(-BigInt(x)))

  override def <(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x < y
      case SafeLongBigInt(y) => x < y
    }

  override def <=(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x <= y
      case SafeLongBigInt(y) => x <= y
    }

  override def >(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x > y
      case SafeLongBigInt(y) => x > y
    }

  override def >=(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x >= y
      case SafeLongBigInt(y) => x >= y
    }

  def compare(that: SafeLong): Int =
    that match {
      case SafeLongLong(y) =>
        x compare y
      case SafeLongBigInt(y) =>
        // y can't be a valid Long, so x != y
        if (x < y) -1 else 1
    }

  def <<(n: Int): SafeLong = {
    if (x == 0) return this
    if (n < 0) return this >> -n
    if (n < 64) {
      if (x >= 0) {
        if (x <= (0x7fffffffffffffffL >> n)) return SafeLongLong(x << n)
      } else {
        if (x >= (0x8000000000000000L >> n)) return SafeLongLong(x << n)
      }
    }
    SafeLongBigInt(BigInt(x) << n)
  }

  def >>(n: Int): SafeLong =
    if (n >= 64) (if (x >= 0) SafeLong.zero else SafeLong(-1))
    else if (n >= 0) SafeLongLong(x >> n)
    else if (n == Int.MinValue) throw new ArithmeticException(">> MinValue not supported")
    else this << -n

  override def equals(that: Any): Boolean =
    that match {
      case SafeLongLong(y) => x == y
      case SafeLongBigInt(y) => x == y
      case that: BigInt => if (that.bitLength > 63) false else that.toLong == x
      case that => that == x
    }

  def abs: SafeLong =
    if (x >= 0) this
    else if (x == Long.MinValue) SafeLong.safe64
    else SafeLong(-x)

  def gcd(that: SafeLong): SafeLong =
    that match {
      case SafeLongLong(y) => SafeLong.longGcd(x, y)
      case SafeLongBigInt(y) => SafeLong.mixedGcd(x, y)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt

  def underlying: java.lang.Long = new java.lang.Long(x)

  override def isValidInt: Boolean = Int.MinValue <= x && x <= Int.MaxValue
  def isValidLong: Boolean = true
  def getLong: Option[Long] = Some(x)

  override def toLong: Long = x
  def toBigInt: BigInt = BigInt(x)
  def toBigDecimal = BigDecimal(x)

  def bitLength: Int = 64 - java.lang.Long.numberOfLeadingZeros(x)
}

case class SafeLongBigInt private[math] (x: BigInt) extends SafeLong {

  def isZero: Boolean = x.signum == 0
  def signum: Int = x.signum

  def +(y: Long): SafeLong =
    if ((x.signum ^ y) < 0) SafeLong(x + y) else SafeLongBigInt(x + y)

  def -(y: Long): SafeLong =
    if ((x.signum ^ y) >= 0) SafeLong(x - y) else SafeLongBigInt(x - y)

  def *(y: Long): SafeLong = SafeLong(x * y)

  def /(y: Long): SafeLong = SafeLong(x / y)

  def %(y: Long): SafeLong = SafeLong(x % y)

  def /%(y: Long): (SafeLong, SafeLong) = {
    val (q, r) = x /% BigInt(y)
    (SafeLong(q), SafeLong(r))
  }

  def &(y: Long): SafeLong = SafeLong(x & y)
  def |(y: Long): SafeLong = SafeLong(x | y)
  def ^(y: Long): SafeLong = SafeLong(x ^ y)

  def +(y: BigInt): SafeLong =
    if ((x.signum ^ y.signum) < 0) SafeLong(x + y) else SafeLongBigInt(x + y)

  def -(y: BigInt): SafeLong =
    if ((x.signum ^ y.signum) < 0) SafeLongBigInt(x - y) else SafeLong(x - y)

  def *(y: BigInt): SafeLong = SafeLong(x * y)

  def /(y: BigInt): SafeLong = SafeLong(x / y)

  def %(y: BigInt): SafeLong = SafeLong(x % y)

  def /%(y: BigInt): (SafeLong, SafeLong) = {
    val (q, r) = x /% y
    (SafeLong(q), SafeLong(r))
  }

  def &(y: BigInt): SafeLong = SafeLong(x & y)
  def |(y: BigInt): SafeLong = SafeLong(x | y)
  def ^(y: BigInt): SafeLong = SafeLong(x ^ y)

  def unary_-(): SafeLong = SafeLong(-x)

  def compare(that: SafeLong): Int =
    that match {
      case SafeLongLong(y) =>
        // x can't be a valid Long, so x != y
        if (x < y) -1 else 1
      case SafeLongBigInt(y) =>
        x compare y
    }

  def <<(n: Int): SafeLong = SafeLong(x << n)
  def >>(n: Int): SafeLong = SafeLong(x >> n)

  override def equals(that: Any): Boolean =
    that match {
      case SafeLongLong(y) => x == y
      case SafeLongBigInt(y) => x == y
      case that: BigInt => x == that
      case that => that == x
    }

  def abs: SafeLong =
    if (x.signum >= 0) this
    else SafeLongBigInt(-x)

  def gcd(that: SafeLong) =
    that match {
      case SafeLongLong(y) => SafeLong.mixedGcd(y, x)
      case SafeLongBigInt(y) => SafeLong(x gcd y)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt

  def underlying: BigInt = x

  override def isValidInt: Boolean = false
  def isValidLong: Boolean = false
  def getLong: Option[Long] = None

  override def toLong: Long = x.toLong
  def toBigInt: BigInt = x
  def toBigDecimal = BigDecimal(x)

  def bitLength: Int = x.bitLength
}

trait SafeLongInstances {
  @SerialVersionUID(1L)
  implicit object SafeLongAlgebra extends SafeLongIsEuclideanRing with SafeLongIsNRoot with Serializable

  @SerialVersionUID(1L)
  implicit object SafeLongIsReal extends SafeLongIsReal with Serializable

  import NumberTag._
  implicit final val SafeLongTag = new LargeTag[SafeLong](Integral, SafeLong.zero)
}

private[math] trait SafeLongIsRing extends Ring[SafeLong] {
  override def minus(a:SafeLong, b:SafeLong): SafeLong = a - b
  def negate(a:SafeLong): SafeLong = -a
  val one: SafeLong = SafeLong.one
  def plus(a:SafeLong, b:SafeLong): SafeLong = a + b
  override def pow(a:SafeLong, b:Int): SafeLong = a pow b
  override def times(a:SafeLong, b:SafeLong): SafeLong = a * b
  val zero: SafeLong = SafeLong.zero

  override def fromInt(n: Int): SafeLong = SafeLong(n)
}

private[math] trait SafeLongIsEuclideanRing extends EuclideanRing[SafeLong] with SafeLongIsRing {
  def quot(a:SafeLong, b:SafeLong): SafeLong = a / b
  def mod(a:SafeLong, b:SafeLong): SafeLong = a % b
  override def quotmod(a:SafeLong, b:SafeLong): (SafeLong, SafeLong) = a /% b
  def gcd(a:SafeLong, b:SafeLong): SafeLong = a gcd b
}

private[math] trait SafeLongIsNRoot extends NRoot[SafeLong] {
  def nroot(a: SafeLong, k: Int): SafeLong =
    a match {
      case SafeLongLong(n) => SafeLong(NRoot[Long].nroot(n, k))
      case SafeLongBigInt(n) => SafeLong(NRoot[BigInt].nroot(n, k))
    }

  def fpow(a: SafeLong, b: SafeLong): SafeLong =
    if (b.isValidInt) a.pow(b.toInt)
    else SafeLong(NRoot[BigInt].fpow(a.toBigInt, b.toBigInt))
}

private[math] trait SafeLongOrder extends Order[SafeLong] {
  override def eqv(x: SafeLong, y: SafeLong) = x == y
  override def neqv(x: SafeLong, y: SafeLong) = x != y
  override def gt(x: SafeLong, y: SafeLong) = x > y
  override def gteqv(x: SafeLong, y: SafeLong) = x >= y
  override def lt(x: SafeLong, y: SafeLong) = x < y
  override def lteqv(x: SafeLong, y: SafeLong) = x <= y
  def compare(x: SafeLong, y: SafeLong) = x compare y
}

private[math] trait SafeLongIsSigned extends Signed[SafeLong] {
  def signum(a: SafeLong): Int = a.signum
  def abs(a: SafeLong): SafeLong = a.abs
}

private[math] trait SafeLongIsReal extends IsIntegral[SafeLong] with SafeLongOrder with SafeLongIsSigned {
  def toDouble(n: SafeLong): Double = n.toDouble
}
