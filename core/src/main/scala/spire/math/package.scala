package spire

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode

import scala.math.ScalaNumericConversions
import BigDecimal.RoundingMode.{CEILING, FLOOR, HALF_UP}
import spire.algebra.{Eq, Field, GCDRing, IsReal, NRoot, Order, Signed, Trig}
import spire.std.bigDecimal._
import spire.syntax.nroot._

import scala.collection.immutable.LazyList

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
    @tailrec def loop(lo: Long, hi: Long, prod: BigInt): BigInt =
      if (lo > hi) prod
      else loop(lo + 1L, hi - 1L, BigInt(lo) * BigInt(hi) * prod)
    if (n < 0) throw new IllegalArgumentException(n.toString)
    else if (n == 0) BigInt(1)
    else if ((n & 1) == 1) loop(1L, n - 1L, BigInt(n))
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
  final def floor[A](a: A)(implicit ev: IsReal[A]): A = ev.floor(a)

  /**
   * round
   */
  final def round(a: Float): Float =
    if (Math.abs(a) >= 16777216.0f) a else Math.round(a).toFloat
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
        doit(precision + 3, leeway * 1000)
      }
    }

    val r = doit(k.mc.getPrecision + 3, 1000)
    new BigDecimal(r.bigDecimal, k.mc)
  }

  final def exp[A](a: A)(implicit t: Trig[A]): A = t.exp(a)

  /**
   * log
   */
  final def log(n: Double): Double = Math.log(n)

  final def log(n: Double, base: Int): Double =
    Math.log(n) / Math.log(base)

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

  def log(n: BigDecimal, base: Int): BigDecimal =
    log(n) / log(BigDecimal(base))

  final def log[A](a: A)(implicit t: Trig[A]): A =
    t.log(a)

  final def log[A](a: A, base: Int)(implicit f: Field[A], t: Trig[A]): A =
    f.div(t.log(a), t.log(f.fromInt(base)))

  /**
   * pow
   */

  // TODO: figure out how much precision we need from log(base) to
  // make the exp() have the right precision
  final def pow(base: BigDecimal, exponent: BigDecimal): BigDecimal =
    if (exponent.abs <= 99999999 && exponent.isWhole)
      base.pow(exponent.toInt)
    else
      exp(log(base) * exponent)

  final def pow(base: BigInt, ex: BigInt): BigInt = {
    @tailrec def bigIntPow(t: BigInt, b: BigInt, e: BigInt): BigInt =
      if (e.signum == 0) t
      else if (e.testBit(0)) bigIntPow(t * b, b * b, e >> 1)
      else bigIntPow(t, b * b, e >> 1)

    if (ex.signum < 0) {
      if (base.signum == 0) throw new ArithmeticException("zero can't be raised to negative power")
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
      if (base == 0L) throw new ArithmeticException("zero can't be raised to negative power")
      else if (base == 1L) 1L
      else if (base == -1L) if ((exponent & 1L) == 0L) -1L else 1L
      else 0L
    } else {
      longPow(1L, base, exponent)
    }
  }

  final def pow(base: Double, exponent: Double): Double = Math.pow(base, exponent)

  /**
   * gcd
   */
  final def gcd(_x: Long, _y: Long): Long = {
    if (_x == 0L) return Math.abs(_y)
    if (_x == 1L) return 1L
    if (_y == 0L) return Math.abs(_x)
    if (_y == 1L) return 1L

    var x = _x
    val xz = numberOfTrailingZeros(x)
    x = Math.abs(x >> xz)

    var y = _y
    val yz = numberOfTrailingZeros(y)
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
  final def gcd[A: Eq](x: A, y: A)(implicit ev: GCDRing[A]): A = ev.gcd(x, y)
  final def gcd[A: Eq](xs: Seq[A])(implicit ev: GCDRing[A]): A =
    xs.reduceLeft(ev.gcd)
  final def gcd[A: Eq](x: A, y: A, z: A, rest: A*)(implicit ev: GCDRing[A]): A =
    if (rest.isEmpty) ev.gcd(ev.gcd(x, y), z)
    else ev.gcd(ev.gcd(ev.gcd(x, y), z), gcd(rest))

  /**
   * lcm
   */
  final def lcm(x: Long, y: Long): Long = if (x == 0 || y == 0) 0 else (x / gcd(x, y)) * y
  final def lcm(a: BigInt, b: BigInt): BigInt = if (a.signum == 0 || b.signum == 0) 0 else (a / a.gcd(b)) * b
  final def lcm[A: Eq](x: A, y: A)(implicit ev: GCDRing[A]): A = ev.lcm(x, y)

  /**
   * Integer Euclidean division, equotmod, equot, emod
   */
  def equotmod(a: Byte, b: Byte): (Byte, Byte) = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) (qt.toByte, rt.toByte)
    else if (b > 0) ((qt - 1).toByte, (rt + b).toByte)
    else ((qt + 1).toByte, (rt - b).toByte)
  }
  def equotmod(a: Short, b: Short): (Short, Short) = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) (qt.toShort, rt.toShort)
    else if (b > 0) ((qt - 1).toShort, (rt + b).toShort)
    else ((qt + 1).toShort, (rt - b).toShort)
  }
  def equotmod(a: Int, b: Int): (Int, Int) = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) (qt, rt)
    else if (b > 0) (qt - 1, rt + b)
    else (qt + 1, rt - b)
  }
  def equotmod(a: Long, b: Long): (Long, Long) = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) (qt, rt)
    else if (b > 0) (qt - 1, rt + b)
    else (qt + 1, rt - b)
  }
  def equotmod(a: BigInt, b: BigInt): (BigInt, BigInt) = {
    val (qt, rt) = a /% b // truncated quotient and remainder
    if (rt.signum >= 0) (qt, rt)
    else if (b.signum > 0) (qt - 1, rt + b)
    else (qt + 1, rt - b)
  }
  def equotmod(a: BigInteger, b: BigInteger): (BigInteger, BigInteger) = {
    val arr = a.divideAndRemainder(b) // truncated quotient and remainder
    val qt = arr(0)
    val rt = arr(1)
    if (rt.signum >= 0) (qt, rt)
    else if (b.signum > 0) (qt.subtract(BigInteger.ONE), rt.add(b))
    else (qt.add(BigInteger.ONE), rt.subtract(b))
  }

  def equot(a: Byte, b: Byte): Byte = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) qt.toByte
    else if (b > 0) (qt - 1).toByte
    else (qt + 1).toByte
  }
  def equot(a: Short, b: Short): Short = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) qt.toShort
    else if (b > 0) (qt - 1).toShort
    else (qt + 1).toShort
  }
  def equot(a: Int, b: Int): Int = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) qt
    else if (b > 0) qt - 1
    else qt + 1
  }
  def equot(a: Long, b: Long): Long = {
    val qt = a / b // truncated quotient
    val rt = a % b // truncated remainder
    if (rt >= 0) qt
    else if (b > 0) qt - 1
    else qt + 1
  }
  def equot(a: BigInt, b: BigInt): BigInt = {
    val (qt, rt) = a /% b // truncated quotient and remainder
    if (rt.signum >= 0) qt
    else if (b.signum > 0) qt - 1
    else qt + 1
  }
  def equot(a: BigInteger, b: BigInteger): BigInteger = {
    val arr = a.divideAndRemainder(b) // truncated quotient and remainder
    val qt = arr(0)
    val rt = arr(1)
    if (rt.signum >= 0) qt
    else if (b.signum > 0) qt.subtract(BigInteger.ONE)
    else qt.add(BigInteger.ONE)
  }

  def emod(a: Byte, b: Byte): Byte = {
    val rt = a % b // truncated remainder
    if (rt >= 0) rt.toByte
    else if (b > 0) (rt + b).toByte
    else (rt - b).toByte
  }
  def emod(a: Short, b: Short): Short = {
    val rt = a % b // truncated remainder
    if (rt >= 0) rt.toShort
    else if (b > 0) (rt + b).toShort
    else (rt - b).toShort
  }
  def emod(a: Int, b: Int): Int = {
    val rt = a % b // truncated remainder
    if (rt >= 0) rt
    else if (b > 0) rt + b
    else rt - b
  }
  def emod(a: Long, b: Long): Long = {
    val rt = a % b // truncated remainder
    if (rt >= 0) rt
    else if (b > 0) rt + b
    else rt - b
  }
  def emod(a: BigInt, b: BigInt): BigInt = {
    val rt = a % b // truncated remainder
    if (rt.signum >= 0) rt
    else if (b > 0) rt + b
    else rt - b
  }
  def emod(a: BigInteger, b: BigInteger): BigInteger = {
    val rt = a.remainder(b) // truncated remainder
    if (rt.signum >= 0) rt
    else if (b.signum > 0) rt.add(b)
    else rt.subtract(b)
  }

  /**
   * min
   */
  final def min(x: Byte, y: Byte): Byte = Math.min(x, y).toByte
  final def min(x: Short, y: Short): Short = Math.min(x, y).toShort
  final def min(x: Int, y: Int): Int = Math.min(x, y)
  final def min(x: Long, y: Long): Long = Math.min(x, y)
  final def min(x: Float, y: Float): Float = Math.min(x, y)
  final def min(x: Double, y: Double): Double = Math.min(x, y)
  final def min[A](x: A, y: A)(implicit ev: Order[A]): A = ev.min(x, y)

  /**
   * max
   */
  final def max(x: Byte, y: Byte): Byte = Math.max(x, y).toByte
  final def max(x: Short, y: Short): Short = Math.max(x, y).toShort
  final def max(x: Int, y: Int): Int = Math.max(x, y)
  final def max(x: Long, y: Long): Long = Math.max(x, y)
  final def max(x: Float, y: Float): Float = Math.max(x, y)
  final def max(x: Double, y: Double): Double = Math.max(x, y)
  final def max[A](x: A, y: A)(implicit ev: Order[A]): A = ev.max(x, y)

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
  final def e[@sp(Float, Double) A](implicit ev: Trig[A]): A = ev.e

  /**
   * pi
   */
  final def pi: Double = Math.PI
  final def pi[@sp(Float, Double) A](implicit ev: Trig[A]): A = ev.pi

  final def sin[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.sin(a)
  final def cos[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.cos(a)
  final def tan[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.tan(a)

  final def asin[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.asin(a)
  final def acos[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.acos(a)
  final def atan[@sp(Float, Double) A](a: A)(implicit ev: Trig[A]): A = ev.atan(a)
  final def atan2[@sp(Float, Double) A](y: A, x: A)(implicit ev: Trig[A]): A = ev.atan2(y, x)

  final def sinh[@sp(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.sinh(x)
  final def cosh[@sp(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.cosh(x)
  final def tanh[@sp(Float, Double) A](x: A)(implicit ev: Trig[A]): A = ev.tanh(x)

  // java.lang.Math/scala.math.compatibility
  final def cbrt(x: Double): Double = Math.cbrt(x)
  final def copySign(m: Double, s: Double): Double = Math.copySign(m, s)
  final def copySign(m: Float, s: Float): Float = Math.copySign(m, s)
  final def cosh(x: Double): Double = Math.cosh(x)
  final def expm1(x: Double): Double = Math.expm1(x)
  final def getExponent(x: Double): Int = Math.getExponent(x)
  final def getExponent(x: Float): Int = Math.getExponent(x)
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

  final def hypot[@sp(Float, Double) A](x: A, y: A)(implicit f: Field[A], n: NRoot[A], s: Signed[A]): A = {
    import spire.implicits._

    def abs(n: A): A = if (n < f.zero) -n else n
    val ax = abs(x)
    val ay = abs(y)
    if (x == f.zero) ay
    else if (y == f.zero) ax
    else if (ax > ay) ax * (1 + (y / x) ** 2).sqrt
    else ay * (1 + (x / y) ** 2).sqrt
  }

  // BigInt
  /**
   * This will return the largest integer that meets some criteria. Specifically,
   * if we're looking for some integer `x` and `f(x')` is guaranteed to return
   * `true` iff `x' <= x`, then this will return `x`.
   *
   * This can be used, for example, to find an integer `x` s.t.
   * `x * x < y < (x+1)*(x+1)`, by using `intSearch(x => x * x <= y)`.
   */
  private def intSearch(f: Int => Boolean): Int = {
    val ceil = (0 until 32).find(i => !f(1 << i)).getOrElse(33)
    if (ceil == 0) {
      0
    } else {
      ((ceil - 1) to 0 by -1).foldLeft(0) { (x, i) =>
        val y = x | (1 << i)
        if (f(y)) y else x
      }
    }
  }

  /**
   * Returns the digits to the right of the decimal point of `x / y` in base
   * `r` if x < y.
   */
  private def decDiv(x: BigInt, y: BigInt, r: Int): LazyList[BigInt] = {
    val expanded = x * r
    val quot = expanded / y
    val rem = expanded - (quot * y)

    if (rem == 0) {
      quot #:: LazyList.empty
    } else {
      quot #:: decDiv(rem, y, r)
    }
  }

  /**
   * Returns the digits of `x` in base `r`.
   */
  private def digitize(x: BigInt, r: Int, prev: List[Int] = Nil): List[Int] =
    if (x == 0) prev else digitize(x / r, r, (x % r).toInt :: prev)

  /**
   * Converts a list of digits in base `r` to a `BigInt`.
   */
  private def undigitize(digits: Seq[Int], r: Int): BigInt =
    digits.foldLeft(BigInt(0))(_ * r + _)

  // 1 billion: because it's the largest positive Int power of 10.
  private val radix = 1000000000

  /**
   * An implementation of the shifting n-th root algorithm for BigDecimal. For
   * the BigDecimal a, this is guaranteed to be accurate up to the precision
   * specified in ctxt.
   *
   * See http://en.wikipedia.org/wiki/Shifting_nth_root_algorithm
   *
   * @param a A (positive if k % 2 == 0) `BigDecimal`.
   * @param k A positive `Int` greater than 1.
   * @param ctxt The `MathContext` to bound the precision of the result.
   *
   * returns A `BigDecimal` approximation to the `k`-th root of `a`.
   */
  def nroot(a: BigDecimal, k: Int, ctxt: MathContext): BigDecimal =
    if (k == 0) {
      BigDecimal(1)
    } else if (a.signum < 0) {
      if (k % 2 == 0) {
        throw new ArithmeticException("%d-root of negative number".format(k))
      } else {
        -nroot(-a, k, ctxt)
      }
    } else {
      val underlying = BigInt(a.bigDecimal.unscaledValue.toByteArray)
      val scale = BigInt(10).pow(a.scale)
      val intPart = digitize(underlying / scale, radix)
      val fracPart = decDiv(underlying % scale, scale, radix).map(_.toInt)
      val leader =
        if (intPart.size % k == 0) LazyList.empty
        else {
          LazyList.fill(k - intPart.size % k)(0)
        }
      val digits = leader ++ LazyList.from(intPart) ++ fracPart ++ LazyList.continually(0)
      val radixPowK = BigInt(radix).pow(k)

      // Total # of digits to compute.
      // Note: I originally had `+ 1` here, but some edge cases were missed, so now
      // it is `+ 2`.
      val maxSize = (ctxt.getPrecision + 8) / 9 + 2

      def findRoot(digits: LazyList[Int], y: BigInt, r: BigInt, i: Int): (Int, BigInt) = {
        val y_ = y * radix
        val a = undigitize(digits.take(k), radix)
        // Note: target grows quite fast (so I imagine (y_ + b) pow k does too).
        val target = radixPowK * r + a + (y_.pow(k))
        val b = intSearch(b => ((y_ + b).pow(k)) <= target)

        val ny = y_ + b

        if (i == maxSize) {
          (i, ny)
        } else {
          val nr = target - (ny.pow(k))

          // TODO: Add stopping condition for when nr == 0 and there are no more
          // digits. Tricky part is refactoring to know when digits end...

          findRoot(digits.drop(k), ny, nr, i + 1)
        }
      }

      val (size, unscaled) = findRoot(digits, 0, 0, 1)
      val newscale = (size - (intPart.size + k - 1) / k) * 9
      BigDecimal(unscaled, newscale, ctxt)
    }

  // ugly internal scala.math.ScalaNumber utilities follow

  private[spire] def anyIsZero(n: Any): Boolean =
    n match {
      case x if x == 0                => true
      case c: ScalaNumericConversions => c.isValidInt && c.toInt == 0
      case _                          => false
    }

  private[spire] def anyToDouble(n: Any): Double =
    n match {
      case n: Byte                    => n.toDouble
      case n: Short                   => n.toDouble
      case n: Char                    => n.toDouble
      case n: Int                     => n.toDouble
      case n: Long                    => n.toDouble
      case n: Float                   => n.toDouble
      case n: Double                  => n
      case c: ScalaNumericConversions => c.toDouble
      case _                          => throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
    }

  private[spire] def anyToLong(n: Any): Long =
    n match {
      case n: Byte                    => n.toLong
      case n: Short                   => n.toLong
      case n: Char                    => n.toLong
      case n: Int                     => n.toLong
      case n: Long                    => n
      case n: Float                   => n.toLong
      case n: Double                  => n.toLong
      case c: ScalaNumericConversions => c.toLong
      case _                          => throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
    }

  private[spire] def anyIsWhole(n: Any): Boolean =
    n match {
      case _: Byte                    => true
      case _: Short                   => true
      case _: Char                    => true
      case _: Int                     => true
      case _: Long                    => true
      case n: Float                   => n.isWhole
      case n: Double                  => n.isWhole
      case c: ScalaNumericConversions => c.isWhole
      case _                          => throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
    }

  private[spire] def anyIsValidInt(n: Any): Boolean =
    n match {
      case _: Byte                    => true
      case _: Short                   => true
      case _: Char                    => true
      case _: Int                     => true
      case n: Long                    => n.isValidInt
      case n: Float                   => n.isValidInt
      case n: Double                  => n.isValidInt
      case c: ScalaNumericConversions => c.isValidInt
      case _                          => throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
    }
}
