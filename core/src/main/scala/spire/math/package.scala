package spire

import spire.algebra._
import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}

import spire.std.bigDecimal._
import spire.syntax.nroot._

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode

import BigDecimal.RoundingMode.{FLOOR, HALF_UP, CEILING}

package object math {

  /**
   * abs
   */
  final def abs(n: Byte): Byte = Math.abs(n).toByte
  final def abs(n: Short): Short = Math.abs(n).toShort
  final def abs(n: Int): Int = Math.abs(n)
  final def abs(n: Long): Long = Math.abs(n)
  final def abs(n: Float): Float = Math.abs(n)
  final def abs(n: Double): Double = Math.abs(n)
  final def abs[A](a: A)(implicit ev: Signed[A]): A = ev.abs(a)

  /**
   * ceil
   */
  final def ceil(n: Float): Float = Math.ceil(n).toFloat
  final def ceil(n: Double): Double = Math.ceil(n)
  final def ceil(n: BigDecimal): BigDecimal = n.setScale(0, CEILING)
  final def ceil[A](a: A)(implicit ev: IsReal[A]): A = ev.ceil(a)

  /**
   * choose (binomial coefficient)
   */
  def choose(n: Long, k: Long): BigInt = {
    if (n < 0 || k < 0) throw new IllegalArgumentException(s"n=$n, k=$k")
    if (k == 0L || k == n) return BigInt(1)
    if (k > n) return BigInt(0)
    if (n - k > k) return choose(n, n - k)

    @tailrec def loop(lo: Long, hi: Long, prod: BigInt): BigInt =
      if (lo > hi) prod
      else loop(lo + 1L, hi - 1L, BigInt(lo) * BigInt(hi) * prod)

    if (((n - k) & 1) == 1)
      loop(k + 1, n - 1L, BigInt(n)) / fact(n - k)
    else
      loop(k + 1, n, BigInt(1)) / fact(n - k)
  }

  /**
   * factorial
   */
  def fact(n: Long): BigInt = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    @tailrec def loop(lo: Long, hi: Long, prod: BigInt): BigInt =
      if (lo > hi) prod
      else loop(lo + 1L, hi - 1L, BigInt(lo) * BigInt(hi) * prod)
    if ((n & 1) == 1) loop(1L, n - 1L, BigInt(n))
    else loop(2L, n - 1L, BigInt(n))
  }

  /**
   * fibonacci
   */
  def fib(n: Long): BigInt = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    var i = 63
    while (((n >>> i) & 1) == 0 && i >= 0) i -= 1
    @tailrec def loop(a: BigInt, b: BigInt, i: Int): BigInt = {
      val c = a + b
      if (i < 0) b
      else if (((n >>> i) & 1) == 1) loop((a + c) * b, b * b + c * c, i - 1)
      else loop(a * a + b * b, (a + c) * b, i - 1)
    }
    loop(BigInt(1), BigInt(0), i)
  }

  /**
   * floor
   */
  final def floor(n: Float): Float = Math.floor(n).toFloat
  final def floor(n: Double): Double = Math.floor(n)
  final def floor(n: BigDecimal): BigDecimal = n.setScale(0, FLOOR)
  final def floor[A](a: A)(implicit ev: IsReal[A]): A = ev.ceil(a)

  /**
   * round
   */
  final def round(a: Float): Float =
    if (Math.abs(a) >= 16777216.0F) a else Math.round(a).toFloat
  final def round(a: Double): Double =
    if (Math.abs(a) >= 4503599627370496.0) a else Math.round(a).toDouble
  final def round(a: BigDecimal): BigDecimal =
    a.setScale(0, HALF_UP)
  final def round[A](a: A)(implicit ev: IsReal[A]): A = ev.round(a)

  /**
   * exp
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
   * log
   */
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
   * pow
   */

  // TODO: figure out how much precision we need from log(base) to
  // make the exp() have the right precision
  final def pow(base: BigDecimal, exponent: BigDecimal) =
    if (exponent.scale == 0 && 0 <= exponent && exponent <= 999999999)
      base.pow(exponent.toInt)
    else
      exp(log(base) * exponent)

  final def pow(base: BigInt, ex: BigInt) = {
    @tailrec def bigIntPow(t: BigInt, b: BigInt, e: BigInt): BigInt =
      if (e.signum == 0) t
      else if (e.testBit(0)) bigIntPow(t * b, b * b, e >> 1)
      else bigIntPow(t, b * b, e >> 1)

    if (ex.signum < 0) {
      if (base.signum == 0) sys.error("zero can't be raised to negative power")
      else if (base == 1) base
      else if (base == -1) if (ex.testBit(0)) BigInt(1) else base
      else BigInt(0)
    } else if (ex.isValidInt) {
      base.pow(ex.toInt)
    } else {
      bigIntPow(BigInt(1), base, ex)
    }
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

  /**
   * gcd
   */
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
  final def gcd[A](xs: Seq[A])(implicit ev: EuclideanRing[A]): A =
    xs.foldLeft(ev.zero) { (x, y) => gcd(y, x) }
  final def gcd[A](x: A, y: A, z: A, rest: A*)(implicit ev: EuclideanRing[A]): A =
    gcd(gcd(gcd(x, y), z), gcd(rest))

  /**
   * lcm
   */
  final def lcm(x: Long, y: Long): Long = (x / gcd(x, y)) * y
  final def lcm(a: BigInt, b: BigInt): BigInt = (a / a.gcd(b)) * b
  final def lcm[A](x: A, y: A)(implicit ev: EuclideanRing[A]): A = ev.lcm(x, y)

  /**
   * min
   */
  final def min(x: Byte, y: Byte): Byte = Math.min(x, y).toByte
  final def min(x: Short, y: Short): Short = Math.min(x, y).toShort
  final def min(x: Int, y: Int): Int = Math.min(x, y)
  final def min(x: Long, y: Long): Long = Math.min(x, y)
  final def min(x: Float, y: Float): Float = Math.min(x, y)
  final def min(x: Double, y: Double): Double = Math.min(x, y)
  final def min[A](x: A, y: A)(implicit ev: Order[A]) = ev.min(x, y)

  /**
   * max
   */
  final def max(x: Byte, y: Byte): Byte = Math.max(x, y).toByte
  final def max(x: Short, y: Short): Short = Math.max(x, y).toShort
  final def max(x: Int, y: Int): Int = Math.max(x, y)
  final def max(x: Long, y: Long): Long = Math.max(x, y)
  final def max(x: Float, y: Float): Float = Math.max(x, y)
  final def max(x: Double, y: Double): Double = Math.max(x, y)
  final def max[A](x: A, y: A)(implicit ev: Order[A]) = ev.max(x, y)

  /**
   * signum
   */
  final def signum(x: Double): Double = Math.signum(x)
  final def signum(x: Float): Float = Math.signum(x)
  final def signum[A](a: A)(implicit ev: Signed[A]): Int = ev.signum(a)

  /**
   * sqrt
   */
  final def sqrt(x: Double): Double = Math.sqrt(x)
  final def sqrt[A](a: A)(implicit ev: NRoot[A]): A = ev.sqrt(a)

  /**
   * e
   */
  final def e: Double = Math.E
  final def e[@spec(Float, Double) A](implicit ev: Trig[A]): A = ev.e

  /**
   * pi
   */
  final def pi: Double = Math.PI
  final def pi[@spec(Float, Double) A](implicit ev: Trig[A]): A = ev.pi

  final def sin[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.sin(a)
  final def cos[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.cos(a)
  final def tan[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.tan(a)

  final def asin[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.asin(a)
  final def acos[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.acos(a)
  final def atan[@spec(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.atan(a)
  final def atan2[@spec(Float, Double) A](y: A, x: A)(implicit ev: Trig[A]): A = ev.atan2(y, x)

  final def sinh[@spec(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.sinh(x)
  final def cosh[@spec(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.cosh(x)
  final def tanh[@spec(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.tanh(x)

  // java.lang.Math/scala.math.compatibility
  final def cbrt(x: Double): Double = Math.cbrt(x)
  final def copySign(m: Double, s: Double): Double = Math.copySign(m, s)
  final def copySign(m: Float, s: Float): Float = Math.copySign(m, s)
  final def cosh(x: Double): Double = Math.cosh(x)
  final def expm1(x: Double): Double = Math.expm1(x)
  final def getExponent(x: Double): Int = Math.getExponent(x)
  final def getExponent(x: Float): Int = Math.getExponent(x)
  final def hypot(x: Double, y: Double): Double = Math.hypot(x, y)
  final def IEEEremainder(x: Double, d: Double): Double = Math.IEEEremainder(x, d)
  final def log10(x: Double): Double = Math.log10(x)
  final def log1p(x: Double): Double = Math.log1p(x)
  final def nextAfter(x: Double, y: Double): Double = Math.nextAfter(x, y)
  final def nextAfter(x: Float, y: Float): Float = Math.nextAfter(x, y)
  final def nextUp(x: Double): Double = Math.nextUp(x)
  final def nextUp(x: Float): Float = Math.nextUp(x)
  final def random(): Double = Math.random()
  final def rint(x: Double): Double = Math.rint(x)
  final def scalb(d: Double, s: Int): Double = Math.scalb(d, s)
  final def scalb(d: Float, s: Int): Float = Math.scalb(d, s)
  final def toDegrees(a: Double): Double = Math.toDegrees(a)
  final def toRadians(a: Double): Double = Math.toRadians(a)
  final def ulp(x: Double): Double = Math.ulp(x)
  final def ulp(x: Float): Double = Math.ulp(x)
}
