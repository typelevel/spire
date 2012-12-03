package spire.algebra

import spire.math._
import spire.macrosk.Ops

import scala.{specialized => spec, math => mth}
import java.math.MathContext
import java.lang.Math

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
trait NRoot[@spec(Double,Float,Int,Long) A] {
  def nroot(a: A, n: Int): A
  def sqrt(a: A): A = nroot(a, 2)
  def log(a:A):A
  def fpow(a:A, b:A): A
}

final class NRootOps[A](lhs: A)(implicit n: NRoot[A]) {
  def nroot(rhs: Int): A = macro Ops.binop[Int, A]
  def sqrt(): A = macro Ops.unop[A]
  def log(): A = macro Ops.unop[A]
  def fpow(rhs: A): A = macro Ops.binop[A, A]
}

trait DoubleIsNRoot extends NRoot[Double] {
  def nroot(a: Double, k: Int): Double = Math.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = Math.sqrt(a)
  def log(a: Double) = Math.log(a)
  def fpow(a: Double, b: Double) = Math.pow(a, b)
}

trait FloatIsNRoot extends NRoot[Float] {
  def nroot(a: Float, k: Int): Float = Math.pow(a, 1 / k.toDouble).toFloat
  override def sqrt(a: Float): Float = Math.sqrt(a).toFloat
  def log(a: Float) = Math.log(a).toFloat
  def fpow(a: Float, b: Float) = Math.pow(a, b).toFloat
}


trait RationalIsNRoot extends NRoot[Rational] {
  implicit def context:ApproximationContext[Rational]
  def nroot(a: Rational, k: Int): Rational = a.nroot(k)
  def log(a: Rational): Rational = a.log
  def fpow(a: Rational, b: Rational): Rational = a.pow(b)
}

trait RealIsNRoot extends NRoot[Real] {
  def nroot(a: Real, k: Int): Real = a nroot k
  def log(a:Real) = sys.error("fixme")
  def fpow(a:Real, b:Real) = sys.error("fixme")
}


trait BigDecimalIsNRoot extends NRoot[BigDecimal] {
  def nroot(a: BigDecimal, k: Int): BigDecimal = {
    if (a.mc.getPrecision <= 0)
      throw new ArithmeticException("Cannot find the nroot of a BigDecimal with unlimited precision.")
    NRoot.nroot(a, k, a.mc)
  }
  def log(a:BigDecimal) = fun.log(a)
  def fpow(a:BigDecimal, b:BigDecimal) = fun.pow(a, b)
}


trait IntIsNRoot extends NRoot[Int] {
  def nroot(x: Int, n: Int): Int = {
    def findnroot(prev: Int, add: Int): Int = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a:Int) = Math.log(a.toDouble).toInt
  def fpow(a:Int, b:Int) = Math.pow(a, b).toInt
}

trait LongIsNRoot extends NRoot[Long] {
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1L << ((65 - n) / n))
  }
  def log(a:Long) = Math.log(a.toDouble).toLong
  def fpow(a:Long, b:Long) = fun.pow(a, b) // xyz
}

trait BigIntIsNRoot extends NRoot[BigInt] {
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
  def log(a:BigInt) = fun.log(BigDecimal(a)).toBigInt
  def fpow(a:BigInt, b:BigInt) = fun.pow(BigDecimal(a), BigDecimal(b)).toBigInt
}

trait SafeLongIsNRoot extends NRoot[SafeLong] {
  import NRoot.{LongIsNRoot, BigIntIsNRoot}

  def nroot(a: SafeLong, k: Int): SafeLong = a.fold(
    n => SafeLong(LongIsNRoot.nroot(n, k)),
    n => SafeLong(BigIntIsNRoot.nroot(n, k))
  )
  def log(a:SafeLong) = a.fold(
    n => SafeLong(LongIsNRoot.log(n)),
    n => SafeLong(BigIntIsNRoot.log(n))
  )

  def fpow(a:SafeLong, b:SafeLong) =
    SafeLong(BigIntIsNRoot.fpow(a.toBigInt, b.toBigInt))
}

object NRoot {
  @inline final def apply[@spec(Int,Long,Float,Double) A](implicit ev:NRoot[A]) = ev

  implicit object IntIsNRoot extends IntIsNRoot
  implicit object LongIsNRoot extends LongIsNRoot
  implicit object BigIntIsNRoot extends BigIntIsNRoot
  implicit object SafeLongIsNRoot extends SafeLongIsNRoot

  implicit object FloatIsNRoot extends FloatIsNRoot
  implicit object DoubleIsNRoot extends DoubleIsNRoot
  implicit object BigDecimalIsNRoot extends BigDecimalIsNRoot

  implicit def rationalIsNRoot(implicit c:ApproximationContext[Rational]) = new RationalIsNRoot {
    implicit def context = c
  }

  implicit object RealIsNRoot extends RealIsNRoot

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
