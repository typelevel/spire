package spire
package math

import java.math.BigInteger

import spire.util.Opt

import scala.math.{ScalaNumber, ScalaNumericConversions}
import spire.macros.Checked

import spire.algebra._
import spire.std.long._
import spire.std.bigInteger._

//scalastyle:off equals.hash.code
/**
 * Provides a type to do safe long arithmetic. This type will never overflow,
 * but rather convert the underlying long to a BigInteger as need and back down
 * to a Long when possible.
 */
sealed abstract class SafeLong extends ScalaNumber with ScalaNumericConversions with Ordered[SafeLong] { lhs =>

  def isZero: Boolean

  def isOne: Boolean

  def signum: Int

  final def +(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs + n
      case SafeLongBigInteger(n) => lhs + n
    }

  final def -(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs - n
      case SafeLongBigInteger(n) => lhs - n
    }

  final def *(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs * n
      case SafeLongBigInteger(n) => lhs * n
    }

  final def /(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs / n
      case SafeLongBigInteger(n) => lhs / n
    }

  final def %(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs % n
      case SafeLongBigInteger(n) => lhs % n
    }

  final def /~(rhs: SafeLong): SafeLong =
    lhs / rhs

  final def /%(rhs: SafeLong): (SafeLong, SafeLong) =
    rhs match {
      case SafeLongLong(n) => lhs /% n
      case SafeLongBigInteger(n) => lhs /% n
    }

  final def &(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs & n
      case SafeLongBigInteger(n) => lhs & n
    }

  final def |(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs | n
      case SafeLongBigInteger(n) => lhs | n
    }

  final def ^(rhs: SafeLong): SafeLong =
    rhs match {
      case SafeLongLong(n) => lhs ^ n
      case SafeLongBigInteger(n) => lhs ^ n
    }

  def ===(that: SafeLong): Boolean =
    this == that

  def =!=(that: SafeLong): Boolean =
    !(this === that)

  def +(rhs: Long): SafeLong
  def -(rhs: Long): SafeLong
  def *(rhs: Long): SafeLong
  def /(rhs: Long): SafeLong
  def %(rhs: Long): SafeLong
  def /%(rhs: Long): (SafeLong, SafeLong)
  def &(rhs: Long): SafeLong
  def |(rhs: Long): SafeLong
  def ^(rhs: Long): SafeLong

  final def +(rhs: BigInt): SafeLong = this + rhs.bigInteger
  final def -(rhs: BigInt): SafeLong = this - rhs.bigInteger
  final def *(rhs: BigInt): SafeLong = this * rhs.bigInteger
  final def /(rhs: BigInt): SafeLong = this / rhs.bigInteger
  final def %(rhs: BigInt): SafeLong = this % rhs.bigInteger
  final def /%(rhs: BigInt): (SafeLong, SafeLong) =  this /% rhs.bigInteger
  final def &(rhs: BigInt): SafeLong = this & rhs.bigInteger
  final def |(rhs: BigInt): SafeLong = this | rhs.bigInteger
  final def ^(rhs: BigInt): SafeLong = this ^ rhs.bigInteger

  private[math] def +(rhs: BigInteger): SafeLong
  private[math] def -(rhs: BigInteger): SafeLong
  private[math] def *(rhs: BigInteger): SafeLong
  private[math] def /(rhs: BigInteger): SafeLong
  private[math] def %(rhs: BigInteger): SafeLong
  private[math] def /%(rhs: BigInteger): (SafeLong, SafeLong)
  private[math] def &(rhs: BigInteger): SafeLong
  private[math] def |(rhs: BigInteger): SafeLong
  private[math] def ^(rhs: BigInteger): SafeLong

  final def min(that: SafeLong): SafeLong =
    if (this < that) this else that

  final def max(that: SafeLong): SafeLong =
    if (this > that) this else that

  def <<(n: Int): SafeLong
  def >>(n: Int): SafeLong

  /**
   * Exponentiation function, e.g. x ** y
   *
   * If base ** exponent doesn't fit in a Long, the result will overflow (unlike
   * scala.math.pow which will return +/- Infinity).
   */
  final def **(k: Int):SafeLong = pow(k)

  final def pow(k: Int): SafeLong = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    @tailrec def loop(total: SafeLong, base: SafeLong, exp: Int): SafeLong = {
      if (exp == 0) total
      else if ((exp & 1) == 1) loop(total * base, base * base, exp >> 1)
      else loop(total, base * base, exp >> 1)
    }

    loop(SafeLong.one, this, k)
  }

  final def modPow(k: Int, mod: SafeLong): SafeLong = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    @tailrec def loop(total: SafeLong, base: SafeLong, k: Int, mod: SafeLong): SafeLong = {
      if (k == 0) total
      else if ((k & 1) == 1) loop((total * base) % mod, (base * base) % mod, k >> 1, mod)
      else loop(total, (base * base) % mod, k >> 1, mod)
    }

    loop(SafeLong.one % mod, this, k, mod)
  }

  def abs: SafeLong

  def gcd(that: SafeLong): SafeLong
  def lcm(that: SafeLong): SafeLong = if (this.isZero || that.isZero) SafeLong.zero else (this / (this gcd that)) * that

  def unary_-(): SafeLong

  def isValidLong: Boolean
  def getLong: Opt[Long]

  override def toByte: Byte = toLong.toByte
  override def toShort: Short = toLong.toShort
  override def toInt: Int = toLong.toInt
  final def toBigInt: BigInt = toBigInteger
  def toBigDecimal: BigDecimal
  private[math] def toBigInteger: BigInteger

  override def toString: String =
    this match {
      case SafeLongLong(n) => n.toString
      case SafeLongBigInteger(n) => n.toString
    }

  final def isWhole: Boolean = true

  def isOdd: Boolean

  def isEven: Boolean

  def isPrime: Boolean = prime.isPrime(this)

  def factor: prime.Factors = prime.factor(this)

  /** Returns true if this SafeLong is probably prime, false if it's definitely composite. If certainty is ≤ 0, true is returned.
    * @param certainty a measure of the uncertainty that the caller is willing to tolerate:
    *                  if the call returns true the probability that this BigInteger is
    *                  prime exceeds (1 - 1/2^certainty).
    */
  final def isProbablePrime(certainty: Int): Boolean =
    toBigInteger.isProbablePrime(certainty)

  def bitLength: Int
}

object SafeLong extends SafeLongInstances {

  final val minusOne: SafeLong = SafeLongLong(-1L)
  final val zero: SafeLong = SafeLongLong(0L)
  final val one: SafeLong = SafeLongLong(1L)
  final val two: SafeLong = SafeLongLong(2L)
  final val three: SafeLong = SafeLongLong(3L)
  final val ten: SafeLong = SafeLongLong(10L)

  private[spire] final val big64: BigInteger = BigInteger.ONE.shiftLeft(63)
  private[spire] final val safe64: SafeLong = SafeLong(big64)

  implicit def apply(x: Long): SafeLong = SafeLongLong(x)

  implicit def apply(x: BigInt): SafeLong =
    if (x.isValidLong) SafeLongLong(x.toLong) else SafeLongBigInteger(x.bigInteger)

  private[math] def apply(s: String): SafeLong =
    try {
      SafeLong(java.lang.Long.parseLong(s))
    } catch {
      case _: Exception => SafeLong(new BigInteger(s))
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

  def mixedGcd(x: Long, y: BigInteger): SafeLong =
    if (y.signum == 0) {
      if (x >= 0) SafeLongLong(x)
      else if (x == Long.MinValue) SafeLong.safe64
      else SafeLongLong(-x)
    } else if (x == 0L) {
      SafeLong(y.abs)
    } else if (x == Long.MinValue) {
      SafeLong(SafeLong.big64 gcd y)
    } else {
      SafeLongLong(spire.math.gcd(x, (y remainder BigInteger.valueOf(x)).longValue))
    }
}

private[math] final case class SafeLongLong(x: Long) extends SafeLong {

  def isZero: Boolean = x == 0L
  def isOne: Boolean = x == 1L
  def isOdd: Boolean = (x & 1L) != 0
  def isEven: Boolean = (x & 1L) == 0
  def signum: Int = java.lang.Long.signum(x)

  def +(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x + y))(SafeLongBigInteger(BigInteger.valueOf(x) add BigInteger.valueOf(y)))

  def -(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x - y))(SafeLongBigInteger(BigInteger.valueOf(x) subtract BigInteger.valueOf(y)))

  def *(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x * y))(SafeLongBigInteger(BigInteger.valueOf(x) multiply BigInteger.valueOf(y)))

  def /(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x / y))(SafeLong.safe64)

  def %(y: Long): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(x % y))(SafeLong.zero)

  def /%(y: Long): (SafeLong, SafeLong) =
    if (x == Long.MinValue && y == -1L)
      (SafeLong.safe64, SafeLong.zero)
    else
      (SafeLongLong(x / y), SafeLongLong(x % y))

  def &(y: Long): SafeLong = SafeLongLong(x & y)
  def |(y: Long): SafeLong = SafeLongLong(x | y)
  def ^(y: Long): SafeLong = SafeLongLong(x ^ y)

  def +(y: BigInteger): SafeLong =
    if (y.bitLength <= 63) this + y.longValue
    else SafeLong(BigInteger.valueOf(x) add y)

  def -(y: BigInteger): SafeLong =
    if (y.bitLength <= 63) this - y.longValue
    else SafeLong(BigInteger.valueOf(x) subtract y)

  def *(y: BigInteger): SafeLong =
    if (y.bitLength <= 63) this * y.longValue
    else SafeLong(BigInteger.valueOf(x) multiply y)

  def /(y: BigInteger): SafeLong =
    if (y.bitLength <= 63) this / y.longValue
    else if (x == Long.MinValue && (y equals SafeLong.big64)) SafeLong.minusOne
    else SafeLong.zero

  def %(y: BigInteger): SafeLong =
    if (y.bitLength <= 63) this % y.longValue
    else if (x == Long.MinValue && (y equals SafeLong.big64)) SafeLong.zero
    else this

  def /%(y: BigInteger): (SafeLong, SafeLong) =
    if (y.bitLength <= 63) this /% y.longValue
    else if (x == Long.MinValue && (y equals SafeLong.big64)) (SafeLong.minusOne, SafeLong.zero)
    else (SafeLong.zero, this)

  def &(y: BigInteger): SafeLong = SafeLong(BigInteger.valueOf(x) and y)
  def |(y: BigInteger): SafeLong = SafeLong(BigInteger.valueOf(x) or y)
  def ^(y: BigInteger): SafeLong = SafeLong(BigInteger.valueOf(x) xor y)

  def unary_-(): SafeLong =
    Checked.tryOrReturn[SafeLong](SafeLongLong(-x))(SafeLongBigInteger(BigInteger.valueOf(x).negate()))

  override def <(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x < y
      case SafeLongBigInteger(y) => y.signum > 0
    }

  override def <=(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x <= y
      case SafeLongBigInteger(y) => y.signum > 0
    }

  override def >(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x > y
      case SafeLongBigInteger(y) => y.signum < 0
    }

  override def >=(that: SafeLong): Boolean =
    that match {
      case SafeLongLong(y) => x >= y
      case SafeLongBigInteger(y) => y.signum < 0
    }

  def compare(that: SafeLong): Int =
    that match {
      case SafeLongLong(y) =>
        x compare y
      case SafeLongBigInteger(y) =>
        -y.signum
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
    SafeLongBigInteger(BigInteger.valueOf(x).shiftLeft(n))
  }

  def >>(n: Int): SafeLong =
    if (n >= 64) (if (x >= 0) SafeLong.zero else SafeLong.minusOne)
    else if (n >= 0) SafeLongLong(x >> n)
    else if (n == Int.MinValue) throw new ArithmeticException(">> MinValue not supported")
    else this << -n

  override def equals(that: Any): Boolean =
    that match {
      case SafeLongLong(y) => x == y
      case SafeLongBigInteger(y) => false
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
      case SafeLongBigInteger(y) => SafeLong.mixedGcd(x, y)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt

  def underlying: java.lang.Long = new java.lang.Long(x)
  def isValidLong: Boolean = true
  def getLong: Opt[Long] = Opt(x)

  override def toLong: Long = x
  def toBigInteger: BigInteger = BigInteger.valueOf(x)
  def toBigDecimal: BigDecimal = BigDecimal(x)

  def bitLength: Int = 64 - java.lang.Long.numberOfLeadingZeros(x)
}

private[math] final case class SafeLongBigInteger(x: BigInteger) extends SafeLong {

  def isZero: Boolean = false // 0 will always be represented as a SafeLongLong
  def isOne: Boolean = false // 1 will always be represented as a SafeLongLong
  def isOdd: Boolean = x.testBit(0)
  def isEven: Boolean = !x.testBit(0)
  def signum: Int = x.signum

  def +(y: Long): SafeLong =
    if ((x.signum ^ y) < 0) SafeLong(x add BigInteger.valueOf(y)) else SafeLongBigInteger(x add BigInteger.valueOf(y))

  def -(y: Long): SafeLong =
    if ((x.signum ^ y) >= 0) SafeLong(x subtract BigInteger.valueOf(y)) else SafeLongBigInteger(x subtract BigInteger.valueOf(y))

  def *(y: Long): SafeLong = SafeLong(x multiply BigInteger.valueOf(y))

  def /(y: Long): SafeLong = SafeLong(x divide BigInteger.valueOf(y))

  def %(y: Long): SafeLong = SafeLong(x remainder BigInteger.valueOf(y))

  def /%(y: Long): (SafeLong, SafeLong) = {
    val Array(q, r) = x.divideAndRemainder(BigInteger.valueOf(y))
    (SafeLong(q), SafeLong(r))
  }

  def &(y: Long): SafeLong = SafeLong(x and BigInteger.valueOf(y))
  def |(y: Long): SafeLong = SafeLong(x or BigInteger.valueOf(y))
  def ^(y: Long): SafeLong = SafeLong(x xor BigInteger.valueOf(y))

  def +(y: BigInteger): SafeLong =
    if ((x.signum ^ y.signum) < 0) SafeLong(x add y) else SafeLongBigInteger(x add y)

  def -(y: BigInteger): SafeLong =
    if ((x.signum ^ y.signum) < 0) SafeLongBigInteger(x subtract y) else SafeLong(x subtract y)

  def *(y: BigInteger): SafeLong = SafeLong(x multiply y)

  def /(y: BigInteger): SafeLong = SafeLong(x divide y)

  def %(y: BigInteger): SafeLong = SafeLong(x remainder y)

  def /%(y: BigInteger): (SafeLong, SafeLong) = {
    val Array(q, r) = x divideAndRemainder y
    (SafeLong(q), SafeLong(r))
  }

  def &(y: BigInteger): SafeLong = SafeLong(x and y)
  def |(y: BigInteger): SafeLong = SafeLong(x or y)
  def ^(y: BigInteger): SafeLong = SafeLong(x xor y)

  def unary_-(): SafeLong = SafeLong(x.negate())

  def compare(that: SafeLong): Int =
    that match {
      case SafeLongLong(y) =>
        x.signum
      case SafeLongBigInteger(y) =>
        x compareTo y
    }

  def <<(n: Int): SafeLong = SafeLong(x.shiftLeft(n))
  def >>(n: Int): SafeLong = SafeLong(x.shiftRight(n))

  override def equals(that: Any): Boolean =
    that match {
      case SafeLongLong(y) => false
      case SafeLongBigInteger(y) => x == y
      case that: BigInt => x equals that.bigInteger
      case that => that == BigInt(x)
    }

  def abs: SafeLong =
    if (x.signum >= 0) this
    else SafeLongBigInteger(x.negate())

  def gcd(that: SafeLong): SafeLong =
    that match {
      case SafeLongLong(y) => SafeLong.mixedGcd(y, x)
      case SafeLongBigInteger(y) => SafeLong(x gcd y)
    }

  def doubleValue: Double = x.doubleValue
  def floatValue: Float = x.floatValue
  def longValue: Long = x.longValue
  def intValue: Int = x.intValue
  override def isValidByte: Boolean = false
  override def isValidShort: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidLong: Boolean = false
  override def isValidChar: Boolean = false

  def underlying: BigInt = BigInt(x)

  def getLong: Opt[Long] = Opt.empty[Long]

  override def toLong: Long = x.longValue
  def toBigInteger: BigInteger = x
  def toBigDecimal: BigDecimal = BigDecimal(x)

  def bitLength: Int = x.bitLength
}

trait SafeLongInstances {
  @SerialVersionUID(1L)
  implicit object SafeLongAlgebra extends SafeLongIsEuclideanRing with SafeLongIsUniqueFactorizationDomain
    with SafeLongIsNRoot with Serializable

  @SerialVersionUID(1L)
  implicit object SafeLongIsReal extends SafeLongIsReal with Serializable

  implicit final val SafeLongTag = new NumberTag.LargeTag[SafeLong](NumberTag.Integral, SafeLong.zero)
}

private[math] trait SafeLongIsCRing extends CRing[SafeLong] {
  override def minus(a:SafeLong, b:SafeLong): SafeLong = a - b
  def negate(a:SafeLong): SafeLong = -a
  val one: SafeLong = SafeLong.one
  def plus(a:SafeLong, b:SafeLong): SafeLong = a + b
  override def pow(a:SafeLong, b:Int): SafeLong = a pow b
  override def times(a:SafeLong, b:SafeLong): SafeLong = a * b
  val zero: SafeLong = SafeLong.zero

  override def fromInt(n: Int): SafeLong = SafeLong(n)
}

private[math] trait SafeLongIsGCDRing extends GCDRing[SafeLong] with SafeLongIsCRing {
  def lcm(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a lcm b
  def gcd(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a gcd b
}

private[math] trait SafeLongIsEuclideanRing extends EuclideanRing[SafeLong] with SafeLongIsGCDRing {
  def euclideanFunction(a:SafeLong): BigInt = a.abs.toBigInt
  def equot(a:SafeLong, b:SafeLong): SafeLong = a / b
  def emod(a:SafeLong, b:SafeLong): SafeLong = a % b
  override def equotmod(a:SafeLong, b:SafeLong): (SafeLong, SafeLong) = a /% b
  override def lcm(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a lcm b
  override def gcd(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a gcd b
}

private[math] trait SafeLongIsNRoot extends NRoot[SafeLong] {
  def nroot(a: SafeLong, k: Int): SafeLong =
    a match {
      case SafeLongLong(n) => SafeLong(NRoot[Long].nroot(n, k))
      case SafeLongBigInteger(n) => SafeLong(NRoot[BigInteger].nroot(n, k))
    }

  def fpow(a: SafeLong, b: SafeLong): SafeLong =
    if (b.isValidInt) a.pow(b.toInt)
    else SafeLong(NRoot[BigInteger].fpow(a.toBigInteger, b.toBigInteger))
}

private[math] trait SafeLongOrder extends Order[SafeLong] {
  override def eqv(x: SafeLong, y: SafeLong): Boolean = x == y
  override def neqv(x: SafeLong, y: SafeLong): Boolean = x != y
  override def gt(x: SafeLong, y: SafeLong): Boolean = x > y
  override def gteqv(x: SafeLong, y: SafeLong): Boolean = x >= y
  override def lt(x: SafeLong, y: SafeLong): Boolean = x < y
  override def lteqv(x: SafeLong, y: SafeLong): Boolean = x <= y
  def compare(x: SafeLong, y: SafeLong): Int = x compare y
}

private[math] trait SafeLongSigned extends Signed[SafeLong] with SafeLongOrder {
  override def signum(a: SafeLong): Int = a.signum
  override def abs(a: SafeLong): SafeLong = a.abs
}

private[math] trait SafeLongTruncatedDivision extends TruncatedDivision[SafeLong] with SafeLongSigned {
  def toBigIntOpt(n: SafeLong): Opt[BigInt] = Opt(n.toBigInt)
  def tquot(x: SafeLong, y: SafeLong): SafeLong = x /~ y
  def tmod(x: SafeLong, y: SafeLong): SafeLong = x % y
  override def tquotmod(x: SafeLong, y: SafeLong): (SafeLong, SafeLong) = x /% y

  override def fquotmod(lhs: SafeLong, rhs: SafeLong): (SafeLong, SafeLong) = {
    val (tq, tm) = lhs /% rhs
    val signsDiffer = (tm.signum == -rhs.signum)
    val fq = if (signsDiffer) tq - 1 else tq
    val fm = if (signsDiffer) tm + rhs else tm
    (fq, fm)
  }

   def fquot(lhs: SafeLong, rhs: SafeLong): SafeLong = {
    val (tq, tm) = lhs /% rhs
    if (tm.signum == -rhs.signum) tq - 1 else tq
  }

   def fmod(lhs: SafeLong, rhs: SafeLong): SafeLong = {
    val tm = lhs % rhs
    if (tm.signum == -rhs.signum) tm + rhs else tm
  }
}

private[math] trait SafeLongIsUniqueFactorizationDomain extends UniqueFactorizationDomain[SafeLong]  {
  def isPrime(a: SafeLong): Boolean = a.isPrime
  def factor(a: SafeLong): prime.Factors = a.factor
}

private[math] trait SafeLongIsReal extends IsIntegral[SafeLong] with SafeLongTruncatedDivision {
  def toDouble(n: SafeLong): Double = n.toDouble
  def toBigInt(n: SafeLong): BigInt = n.toBigInt
}
