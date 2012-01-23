package numerics.math

import scala.{specialized => spec, math => mth}
import java.math.MathContext


trait Exponential[@spec(Double) A] {
  def pow(a: A, k: Int): A
  def nroot(a: A, n: Int): A
  def sqrt(a: A): A = nroot(a, 2)
}

object Exponential {
  implicit object DoubleIsExponential extends DoubleIsExponential
  implicit object BigIntIsExponential extends BigIntIsExponential
  implicit object RealIsExponential extends RealIsExponential

  implicit def rationalIsExponential(implicit c: ApproximationContext[Rational]): Exponential[Rational] =
    RationalIsApproxExponential()(c)

  implicit def bigDecimalIsExponential(implicit mc: MathContext): Exponential[BigDecimal] =
    BigDecimalIsApproxExponential()(mc)
}


trait ExponentialOps[@spec(Double) A] {
  val lhs: A
  val exp: Exponential[A]

  def pow(k: Int): A = exp.pow(lhs, k)
  def nroot(k: Int): A = exp.nroot(lhs, k)
  def sqrt: A = exp.sqrt(lhs)
}


trait DoubleIsExponential extends Exponential[Double] {
  def pow(a: Double, k: Int): Double = mth.pow(a, k)
  def nroot(a: Double, k: Int): Double = mth.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = mth.sqrt(a)
}

trait RationalIsExponential extends Exponential[Rational] {
  implicit def context: ApproximationContext[Rational]
  def pow(a: Rational, k: Int): Rational = a pow k
  def nroot(a: Rational, k: Int): Rational = a nroot k
}

case class RationalIsApproxExponential(implicit context: ApproximationContext[Rational]) extends RationalIsExponential

trait BigIntIsExponential extends Exponential[BigInt] {
  def pow(a: BigInt, k: Int): BigInt = a pow k

  def nroot(a: BigInt, k: Int): BigInt = if (a < 0 && k % 2 == 1) {
    -nroot(-a, k)
  } else if (a < 0) {
    throw new ArithmeticException("Cannot find %d-root of negative number." format k)
  } else {
    def findNroot(b: BigInt, i: Int): BigInt = if (i < 0) {
      b
    } else {
      val c = b setBit i

      if ((c pow k) <= a)
        findNroot(c, i - 1)
      else
        findNroot(b, i - 1)
    }

    findNroot(0, a.bitLength - 1)
  }
}

trait RealIsExponential extends Exponential[Real] {
  def pow(a: Real, k: Int): Real = a pow k
  def nroot(a: Real, k: Int): Real = a nroot k
}

trait BigDecimalIsExponential extends Exponential[BigDecimal] {
  def context: MathContext

  def pow(a: BigDecimal, k: Int): BigDecimal = if (k == 1) a else {
    val sq = pow(a, k / 2)
    if (k % 2 == 1) sq * sq * a else sq * sq
  }
  def nroot(a: BigDecimal, k: Int): BigDecimal = NRoots.nroot(a, k, context)
}

case class BigDecimalIsApproxExponential(implicit context: MathContext) extends BigDecimalIsExponential


object NRoots {

  /**
   * This will return the largest integer that meets some criteria. Specifically,
   * if we're looking for some integer `x` and `f(x')` is guaranteed to return
   * `true` iff `x' <= x`, then this will return `x`.
   *
   * This can be used, for example, to find an integer `x` s.t. 
   * `x * x < y < (x+1)*(x+1)`, by using `intSearch(x => x * x <= y)`.
   */
  private def intSearch(f: Int => Boolean): Int = {
    val ceil = (0 until 32) find (i => !f(1 << i)) getOrElse 33
    if (ceil == 0) {
      0
    } else {
      (0 /: ((ceil - 1) to 0 by -1)) { (x, i) =>
        val y = x | (1 << i)
        if (f(y)) y else x
      }
    }
  }


  /**
   * Returns the digits to the right of the decimal point of `x / y` in base
   * `r` if x < y.
   */
  private def decDiv(x: BigInt, y: BigInt, r: Int): Stream[BigInt] = {
    val expanded = x * r
    val quot = expanded / y
    val rem = expanded - (quot * y)

    if (rem == 0) {
      Stream.cons(quot, Stream.empty)
    } else {
      Stream.cons(quot, decDiv(rem, y, r))
    }
  }


  /** Returns the digits of `x` in base `r`. */
  private def digitize(x: BigInt, r: Int, prev: List[Int] = Nil): List[Int] =
    if (x == 0) prev else digitize(x / r, r, (x % r).toInt :: prev)


  /** Converts a list of digits in base `r` to a `BigInt`. */
  private def undigitize(digits: Seq[Int], r: Int): BigInt =
    (BigInt(0) /: digits)(_ * r + _)


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
   * @returns A `BigDecimal` approximation to the `k`-th root of `a`.
   */
  def nroot(a: BigDecimal, k: Int, ctxt: MathContext) = {
    val underlying = BigInt(a.bigDecimal.unscaledValue.toByteArray)
    val scale = BigInt(10) pow a.scale
    val intPart = digitize(underlying / scale, radix)
    val fracPart = decDiv(underlying % scale, scale, radix) map (_.toInt)
    val leader = if (intPart.size % k == 0) Stream.empty else {
      Stream.fill(k - intPart.size % k)(0)
    }
    val digits = leader ++ intPart.toStream ++ fracPart ++ Stream.continually(0)
    val radixPowK = BigInt(radix) pow k

    // Total # of digits to compute.
    val maxSize = (ctxt.getPrecision + 8) / 9 + 1

    def findRoot(digits: Stream[Int], y: BigInt, r: BigInt, i: Int): (Int, BigInt) = {
      val y_ = y * radix
      val a = undigitize(digits take k, radix)
      // Note: target grows quite fast (so I imagine (y_ + b) pow k does too).
      val target = radixPowK * r + a + (y_ pow k)
      val b = intSearch(b => ((y_ + b) pow k) <= target)

      val ny = y_ + b

      if (i == maxSize) {
        (i, ny)
      } else {
        val nr = target - (ny pow k)
        
        // TODO: Add stopping condition for when nr == 0 and there are no more
        // digits. Tricky part is refactoring to know when digits end...

        findRoot(digits drop k, ny, nr, i + 1)
      }
    }

    val (size, unscaled) = findRoot(digits, 0, 0, 1)
    val newscale = (size - (intPart.size + k - 1) / k) * 9
    BigDecimal(unscaled, newscale, ctxt)
  }
}


