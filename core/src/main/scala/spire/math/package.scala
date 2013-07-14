package spire

import spire.algebra._
import spire.math._

import scala.annotation.tailrec

import spire.std.bigDecimal._
import spire.syntax.nroot._

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode

import BigDecimal.RoundingMode.{FLOOR, HALF_UP, CEILING}

package object math {
  // largest possible double as BigDecimal
  private final val maxDouble = BigDecimal(Double.MaxValue)

  // natural log of largest possible double as BigDecimal
  private final val logMaxDouble = BigDecimal(Math.log(Double.MaxValue))

  // e^logMaxDouble as BigDecimal
  private final val expLogMaxDouble = BigDecimal(Math.exp(Math.log(Double.MaxValue)))

  /**
   * log() implementations
   */

  final def abs(n: Byte): Byte = Math.abs(n).toByte
  final def abs(n: Short): Short = Math.abs(n).toShort
  final def abs(n: Int): Int = Math.abs(n)
  final def abs(n: Long): Long = Math.abs(n)
  final def abs(n: Float): Float = Math.abs(n)
  final def abs(n: Double): Double = Math.abs(n)

  final def ceil(n: Float): Float = Math.ceil(n).toFloat
  final def ceil(n: Double): Double = Math.ceil(n)
  final def ceil(n: BigDecimal): BigDecimal = n.setScale(0, CEILING)

  final def floor(n: Float): Float = Math.floor(n).toFloat
  final def floor(n: Double): Double = Math.floor(n)
  final def floor(n: BigDecimal): BigDecimal = n.setScale(0, FLOOR)

  final def log(n: Double): Double = Math.log(n)

  final def log(n: BigDecimal): BigDecimal = {
    val scale = n.mc.getPrecision

    def ln(n: BigDecimal): BigDecimal = {
      val scale2 = scale + 1
      val limit = BigDecimal(5) * BigDecimal(10).pow(-scale2)

      @tailrec def loop(x: BigDecimal): BigDecimal = {
        val xp = exp(x)
        val term = (xp - n) / xp
        if (term > limit) loop(x - term) else x - term
      }

      loop(n.setScale(scale2, HALF_UP)).setScale(scale, HALF_UP)
    }

    if (n.signum < 1)
      throw new IllegalArgumentException("argument <= 0")

    @tailrec def rescale(x: BigDecimal, n: Int): (BigDecimal, Int) =
      if (x < 64) (x, n) else rescale(x.sqrt, n + 1)

    val (x, i) = rescale(n, 0)

    (ln(x) * BigDecimal(2).pow(i)).setScale(scale, HALF_UP)
  }

  final def log[A](a: A)(implicit t: Trig[A]): A = t.log(a)

  /**
   * exp() implementations
   */
  final def exp(n: Double): Double = Math.exp(n)

  final def exp(k: Int, precision: Int): BigDecimal = {
    val mc = new MathContext(precision + 1, RoundingMode.HALF_UP)
    var i = 2
    var num = BigInt(2)
    var denom = BigInt(1)

    val limit = BigInt(10).pow(precision)
    while (denom < limit) {
      denom = denom * i
      num = num * i + BigInt(1)
      i += 1
    }
    val sum = BigDecimal(num, mc) / BigDecimal(denom, mc)
    sum.setScale(precision - sum.precision + sum.scale, FLOOR).pow(k)
  }

  final def exp(k: BigDecimal): BigDecimal = {
    // take a BigDecimal to a BigInt power
    @tailrec
    def power(result: BigDecimal, base: BigDecimal, exponent: BigInt): BigDecimal =
      if (exponent.signum == 0) result
      else if (exponent.testBit(0)) power(result * base, base * base, exponent >> 1)
      else power(result, base * base, exponent >> 1)

    if (k.signum == 0) return BigDecimal(1)

    if (k.signum == -1) return BigDecimal(1) / exp(-k)

    val whole = k.setScale(0, FLOOR)

    if (whole.signum > 1) {
      val part = exp(BigDecimal(1) + (k - whole) / whole)
      return power(BigDecimal(1), part, whole.toBigInt)
    }

    var precision = k.mc.getPrecision + 3
    var leeway = 1000

    @tailrec
    def doit(precision: Int, leeway: Int): BigDecimal = {
      val mc = new MathContext(precision, RoundingMode.HALF_UP)
      var i = 2
      var sum = BigDecimal(1, mc) + k
      var factorial = BigDecimal(2, mc)
      var kpow = k * k
      var term = (kpow / factorial).setScale(precision, HALF_UP)
      while (term.signum != 0 && i < leeway) {
        i += 1
        sum += term
        factorial *= i
        kpow *= k
        term = (kpow / factorial).setScale(precision, HALF_UP)
      }

      if (i <= leeway) {
        sum.setScale(k.mc.getPrecision - sum.precision + sum.scale, FLOOR)
      } else {
        println("  restarted %s" format i)
        doit(precision + 3, leeway * 1000)
      }
    }

    val r = doit(k.mc.getPrecision + 3, 1000)
    BigDecimal(r.bigDecimal, k.mc)
  }

  final def exp[A](a: A)(implicit t: Trig[A]): A = t.exp(a)

  /**
   * pow() implementations
   */

  // TODO: figure out how much precision we need from log(base) to
  // make the exp() have the right precision
  final def pow(base: BigDecimal, exponent: BigDecimal) =
    if (exponent.scale == 0 && 0 <= exponent && exponent <= 999999999)
      base.pow(exponent.toInt)
    else
      exp(log(base) * exponent)

  final def pow(base: BigInt, ex: BigInt) = if (ex.signum < 0) {
    if (base.signum == 0) sys.error("zero can't be raised to negative power")
    else if (base == 1) base
    else if (base == -1) if (ex.testBit(0)) BigInt(1) else base
    else BigInt(0)
  } else if (ex.isValidInt) {
    base.pow(ex.toInt)
  } else {
    bigIntPow(BigInt(1), base, ex)
  }

  @tailrec private[math] final def bigIntPow(t: BigInt, b: BigInt, e: BigInt): BigInt = {
    if (e.signum == 0) t * b.pow(e.toInt)
    else if (e.testBit(0)) bigIntPow(t * b, b * b, e >> 1)
    else bigIntPow(t, b * b, e >> 1)
  }

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^ex doesn't fit in a Long, the result will overflow (unlike
   * Math.pow which will return +/- Infinity). 
   */
  final def pow(base: Long, exponent: Long): Long = {
    @tailrec def longPow(t: Long, b: Long, e: Long): Long =
      if (e == 0L) t
      else if ((e & 1) == 1) longPow(t * b, b * b, e >> 1L)
      else longPow(t, b * b, e >> 1L)

    if (exponent < 0L) {
      if(base == 0L) sys.error("zero can't be raised to negative power")
      else if (base == 1L) 1L
      else if (base == -1L) if ((exponent & 1L) == 0L) -1L else 1L
      else 0L
    } else {
      longPow(1L, base, exponent)
    }
  }

  final def pow(base: Double, exponent: Double) = Math.pow(base, exponent)

  final def gcd(_x: Long, _y: Long): Long = {
    if (_x == 0L) return Math.abs(_y)
    if (_y == 0L) return Math.abs(_x)
  
    var x = _x
    var xz = numberOfTrailingZeros(x)
    x = Math.abs(x >> xz)
  
    var y = _y
    var yz = numberOfTrailingZeros(y)
    y = Math.abs(y >> yz)

    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }

    if (xz < yz) x << xz else x << yz
  }

  final def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  final def gcd[A](x: A, y: A)(implicit ev: EuclideanRing[A]): A = ev.gcd(x, y)

  final def lcm(x: Long, y: Long): Long = (x / gcd(x, y)) * y

  final def lcm(a: BigInt, b: BigInt): BigInt = (a / a.gcd(b)) * b

  final def lcm[A](x: A, y: A)(implicit ev: EuclideanRing[A]): A = ev.lcm(x, y)

  final def round(a: Float): Float =
    if (Math.abs(a) >= 16777216.0F) a else Math.round(a).toFloat

  final def round(a: Double): Double =
    if (Math.abs(a) >= 4503599627370496.0) a else Math.round(a).toDouble

  final def round(a: BigDecimal): BigDecimal =
    a.setScale(0, HALF_UP)

  final def min(x: Byte, y: Byte): Byte = Math.min(x, y).toByte
  final def min(x: Short, y: Short): Short = Math.min(x, y).toShort
  final def min(x: Int, y: Int): Int = Math.min(x, y)
  final def min(x: Long, y: Long): Long = Math.min(x, y)
  final def min(x: Float, y: Float): Float = Math.min(x, y)
  final def min(x: Double, y: Double): Double = Math.min(x, y)
  final def min[A](x: A, y: A)(implicit ev: Order[A]) = ev.min(x, y)

  final def max(x: Byte, y: Byte): Byte = Math.max(x, y).toByte
  final def max(x: Short, y: Short): Short = Math.max(x, y).toShort
  final def max(x: Int, y: Int): Int = Math.max(x, y)
  final def max(x: Long, y: Long): Long = Math.max(x, y)
  final def max(x: Float, y: Float): Float = Math.max(x, y)
  final def max(x: Double, y: Double): Double = Math.max(x, y)
  final def max[A](x: A, y: A)(implicit ev: Order[A]) = ev.max(x, y)

  final def e[A](implicit t: Trig[A]): A = t.e
  final def pi[A](implicit t: Trig[A]): A = t.pi

  final def sin[A](a: A)(implicit t: Trig[A]): A = t.sin(a)
  final def cos[A](a: A)(implicit t: Trig[A]): A = t.cos(a)
  final def tan[A](a: A)(implicit t: Trig[A]): A = t.tan(a)

  final def asin[A](a: A)(implicit t: Trig[A]): A = t.asin(a)
  final def acos[A](a: A)(implicit t: Trig[A]): A = t.acos(a)
  final def atan[A](a: A)(implicit t: Trig[A]): A = t.atan(a)
  final def atan2[A](y: A, x: A)(implicit t: Trig[A]): A = t.atan2(y, x)

  final def sinh[A](x: A)(implicit t: Trig[A]): A = t.sinh(x)
  final def cosh[A](x: A)(implicit t: Trig[A]): A = t.cosh(x)
  final def tanh[A](x: A)(implicit t: Trig[A]): A = t.tanh(x)
}

