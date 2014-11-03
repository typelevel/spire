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

  final def +(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs + n
    case SafeLongBigInt(n) => lhs + n
  }

  final def -(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs - n
    case SafeLongBigInt(n) => lhs - n
  }

  final def *(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs * n
    case SafeLongBigInt(n) => lhs * n
  }

  final def /(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs / n
    case SafeLongBigInt(n) => lhs / n
  }

  final def %(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs % n
    case SafeLongBigInt(n) => lhs % n
  }

  final def /~(rhs: SafeLong): SafeLong = lhs / rhs

  final def /%(rhs: SafeLong): (SafeLong, SafeLong) = rhs match {
    case SafeLongLong(n) => lhs /% n
    case SafeLongBigInt(n) => lhs /% n
  }

  final def &(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs & n
    case SafeLongBigInt(n) => lhs & n
  }

  final def |(rhs: SafeLong): SafeLong = rhs match {
    case SafeLongLong(n) => lhs | n
    case SafeLongBigInt(n) => lhs | n
  }

  final def ^(rhs: SafeLong): SafeLong = rhs match {
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
  final def **(rhs:Int):SafeLong = pow(rhs)

  final def pow(rhs: Int): SafeLong = {
    @tailrec def loop(total: SafeLong, base: SafeLong, exp: Int): SafeLong = {
      if (exp == 0) return total
      else if ((exp & 1) == 1) loop(total * base, base * base, exp >> 1)
      else loop(total, base * base, exp >> 1)
    }

    assert (rhs >= 0)
    loop(SafeLong.one, this, rhs)
  }


  final def modPow(exp: Int, mod: SafeLong): SafeLong = {
    @tailrec def loop(total: SafeLong, base: SafeLong, exp: Int, mod: SafeLong): SafeLong = {
      if (exp == 0) return total
      else if ((exp & 1) == 1) loop((total * base) % mod, (base * base) % mod, exp >> 1, mod)
      else loop(total, (base * base) % mod, exp >> 1, mod)
    }

    assert (exp >= 0)
    loop(SafeLong.one % mod, this, exp, mod)
  }

  final def abs: SafeLong = if (signum < 0) -this else this

  def gcd(that: SafeLong): SafeLong

  def unary_-(): SafeLong

  def isLong: Boolean
  def isValidLong: Boolean

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

  def fold[A, B <: A, C <: A](f: Long => B, g: BigInt => C): A
  
  final def map(f: Long => Long, g: BigInt => BigInt): SafeLong =
    fold(x => SafeLongLong(f(x)), x => SafeLongBigInt(g(x)))
  
  /**
   * If `this` SafeLong is backed by a Long and `that` SafeLong is backed by
   * a Long as well, then `f` will be called with both values. Otherwise,
   * `this` and `that` will be converted to `BigInt`s and `g` will be called
   * with these `BigInt`s.
   */
  final def foldWith[A, B <: A, C <: A](that: SafeLong)(f: (Long, Long) => B, g: (BigInt, BigInt) => C): A =
    fold(x => that.fold(f(x, _), g(BigInt(x), _)), g(_, that.toBigInt))
}


object SafeLong extends SafeLongInstances {
  final val SignBit = 0x8000000000000000L

  final val zero: SafeLong = SafeLongLong(0L)
  final val one: SafeLong = SafeLongLong(1L)
  final val two: SafeLong = SafeLongLong(2L)
  final val three: SafeLong = SafeLongLong(3L)
  final val ten: SafeLong = SafeLongLong(10L)

  implicit def apply(x: Long): SafeLong = SafeLongLong(x)

  implicit def apply(x: BigInt): SafeLong =
    if (x.bitLength > 63) {
      SafeLongBigInt(x)
    } else {
      SafeLongLong(x.toLong)
    }

  def apply(s: String): SafeLong =
    try {
      SafeLong(java.lang.Long.parseLong(s))
    } catch {
      case _: Exception => SafeLong(BigInt(s))
    }
}


private[math] case class SafeLongLong(x: Long) extends SafeLong {

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

  def +(y: BigInt): SafeLong = SafeLong(y + x)
  def -(y: BigInt): SafeLong = SafeLong(BigInt(x) - y)
  def *(y: BigInt): SafeLong = SafeLong(y * x)
  def /(y: BigInt): SafeLong = if (y.bitLength > 63) SafeLong.zero else this / y.toLong
  def %(y: BigInt): SafeLong = if (y.bitLength > 63) x else this % y.toLong
  def /%(y: BigInt) = if (y.bitLength > 63) (SafeLongLong(0), this) else this /% y.toLong

  def &(y: BigInt): SafeLong = SafeLongLong(x & y.toLong)
  def |(y: BigInt): SafeLong = SafeLongBigInt(y | x)
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
      case SafeLongLong(y) => x compare y
      case SafeLongBigInt(y) => if (x < y) -1 else if (x > y) 1 else 0
    }

  def <<(n: Int): SafeLong =
    if (n < 0) this >> -n
    else if (n < 64 && x <= (0x7fffffffffffffffL >> (63 - n))) SafeLongLong(x << n)
    else SafeLongBigInt(BigInt(x) << n)

  def >>(n: Int): SafeLong =
    if (n < 0) this << -n
    else if (n >= 64) SafeLong.zero
    else SafeLongLong(x >> n)

  override def equals(that: Any): Boolean =
    that match {
      case SafeLongLong(y) => x == y
      case SafeLongBigInt(y) => x == y
      case that: BigInt => if (that.bitLength > 63) false else that.toLong == x
      case that => that == x
    }

  def gcd(that: SafeLong): SafeLong =
    if (x == 0) if (that.isZero) SafeLong.one else that
    else that match {
      case SafeLongLong(y) => spire.math.gcd(x, y)
      case SafeLongBigInt(y) => spire.math.gcd(x, (y % x).toLong)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt
  
  def underlying: java.lang.Long = new java.lang.Long(x)

  def isLong: Boolean = true
  override def isValidInt: Boolean = Int.MinValue <= x && x <= Int.MaxValue
  def isValidLong: Boolean = true

  override def toLong: Long = x
  def toBigInt: BigInt = BigInt(x)
  def toBigDecimal = BigDecimal(x)

  def bitLength: Int = 64 - java.lang.Long.numberOfLeadingZeros(x)

  def fold[A,B <: A,C <: A](f: Long => B, g: BigInt => C): A = f(x)
}

private[math] case class SafeLongBigInt(x: BigInt) extends SafeLong {

  def isZero: Boolean = x.signum == 0
  def signum: Int = x.signum

  def +(y: Long): SafeLong = if ((x.signum ^ y) < 0) SafeLong(x + y) else SafeLongBigInt(x + y)
  def -(y: Long): SafeLong = if ((x.signum ^ y) < 0) SafeLongBigInt(x - y) else SafeLong(x - y)
  def *(y: Long): SafeLong = if (y == 0) SafeLong.zero else SafeLongBigInt(x * y)
  def /(y: Long): SafeLong = SafeLong(x / y)
  def %(y: Long): SafeLong = SafeLong(x % y)
  def /%(y: Long): (SafeLong, SafeLong) = {
    val (q, r) = x /% BigInt(y)
    (SafeLong(q), SafeLong(r))
  }

  def &(y: Long): SafeLong = SafeLongLong(x.toLong & y)
  def |(y: Long): SafeLong = SafeLongBigInt(x | y)
  def ^(y: Long): SafeLong = SafeLong(x ^ y)

  def +(y: BigInt): SafeLong = if ((x.signum ^ y.signum) < 0) SafeLong(x + y) else SafeLongBigInt(x + y)
  def -(y: BigInt): SafeLong = if ((x.signum ^ y.signum) < 0) SafeLongBigInt(x - y) else SafeLong(x - y)
  def *(y: BigInt): SafeLong = SafeLongBigInt(x * y)
  def /(y: BigInt): SafeLong = SafeLong(x / y)
  def %(y: BigInt): SafeLong = SafeLong(x % y)
  def /%(y: BigInt): (SafeLong, SafeLong) = {
    val (q, r) = x /% y
    (SafeLong(q), SafeLong(r))
  }

  def &(y: BigInt): SafeLong = SafeLong(x & y)
  def |(y: BigInt): SafeLong = SafeLong(x | y)
  def ^(y: BigInt): SafeLong = SafeLong(x ^ y)
  
  def unary_-(): SafeLong = SafeLong(-x)  // Covers the case where x == Long.MaxValue + 1

  def compare(that: SafeLong): Int =
    that match {
      case SafeLongLong(y) => if (x < y) -1 else if (x > y) 1 else 0
      case SafeLongBigInt(y) => x compare y
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

  def gcd(that: SafeLong) =
    if (isZero) if (that.isZero) SafeLong.one else that
    else that match {
      case SafeLongLong(y) => spire.math.gcd((x % y).toLong, y)
      case SafeLongBigInt(y) => x.gcd(y)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt
  
  def underlying: BigInt = x

  def isLong: Boolean = false
  override def isValidInt: Boolean = x.isValidInt
  def isValidLong: Boolean = x.isValidLong

  override def toLong: Long = x.toLong
  def toBigInt: BigInt = x
  def toBigDecimal = BigDecimal(x)

  def bitLength: Int = x.bitLength

  def fold[A,B <: A,C <: A](f: Long => B, g: BigInt => C): A = g(x)
}

trait SafeLongInstances {
  @SerialVersionUID(1L)
  implicit object SafeLongAlgebra extends SafeLongIsEuclideanRing with SafeLongIsNRoot with Serializable

  @SerialVersionUID(1L)
  implicit object SafeLongIsReal extends SafeLongIsReal with Serializable
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
