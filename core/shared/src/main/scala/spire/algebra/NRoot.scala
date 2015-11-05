package spire
package algebra

import java.math.MathContext

// NOTE: fpow vs pow is a bit of a trainwreck :P
// overloading is evil, but it's definitely what users will expect.

/**
 * This is a type class for types with n-roots. The value returned by `nroot`
 * and `sqrt` are only guaranteed to be approximate answers (except in the case
 * of `Real`).
 *
 * Also, generally `nroot`s where `n` is even are not defined for
 * negative numbers. The behaviour is undefined if this is attempted. It would
 * be nice to ensure an exception is raised, but some types may defer
 * computation and testing if a value is negative may not be ideal. So, do not
 * count on `ArithmeticException`s to save you from bad arithmetic!
 */
trait NRoot[@spec(Double,Float,Int,Long) A] extends Any {
  def nroot(a: A, n: Int): A
  def sqrt(a: A): A = nroot(a, 2)
  def fpow(a:A, b:A): A
}

import spire.math.{ConvertableTo, ConvertableFrom, Number}

object NRoot {
  @inline final def apply[@spec(Int,Long,Float,Double) A](implicit ev:NRoot[A]): NRoot[A] = ev

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
   *
   * returns A `BigDecimal` approximation to the `k`-th root of `a`.
   */
  def nroot(a: BigDecimal, k: Int, ctxt: MathContext): BigDecimal = if (k == 0) {
    BigDecimal(1)
  } else if (a.signum < 0) {
    if (k % 2 == 0) {
      throw new ArithmeticException("%d-root of negative number" format k)
    } else {
      -nroot(-a, k, ctxt)
    }
  } else {
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
    // Note: I originally had `+ 1` here, but some edge cases were missed, so now
    // it is `+ 2`.
    val maxSize = (ctxt.getPrecision + 8) / 9 + 2

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
