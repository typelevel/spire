package spire.algebra

import spire.math._

import scala.{specialized => spec, math => mth}
import java.math.MathContext

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


/**
 * A type class for `EuclideanRing`s with `NRoot`s as well. Since the base
 * requirement is only a `EuclideanRing`, we can provide instances for `Int`,
 * `Long`, and `BigInt`.
 */
trait EuclideanRingWithNRoot[@spec(Int,Long) A] extends EuclideanRing[A] with NRoot[A]


/**
 * A type class for `Field`s with `NRoot`s. These will be `Field`s that have an
 * additional `nroot` and `sqrt` function.
 */
trait FieldWithNRoot[@spec(Double, Float) A] extends EuclideanRingWithNRoot[A] with Field[A]


object EuclideanRingWithNRoot {
  implicit object IntIsEuclideanRingWithNRoot extends EuclideanRingWithNRoot[Int]
                                              with IntIsNRoot with IntIsEuclideanRing
  implicit object LongIsEuclideanRingWithNRoot extends EuclideanRingWithNRoot[Long]
                                               with LongIsNRoot with LongIsEuclideanRing
  implicit object BigIntIsEuclideanRingWithNRoot extends EuclideanRingWithNRoot[BigInt]
                                                 with BigIntIsNRoot
                                                 with BigIntIsEuclideanRing

  implicit def FieldWithNRootIsEuclideanRingWithNRoot[A]
  (implicit e: FieldWithNRoot[A]): EuclideanRingWithNRoot[A] = e

  def apply[A](implicit e: EuclideanRingWithNRoot[A]): EuclideanRingWithNRoot[A] = e
}


object FieldWithNRoot {
  implicit object DoubleIsFieldWithNRoot extends FieldWithNRoot[Double]
                                         with DoubleIsNRoot with DoubleIsField
  implicit object FloatIsFieldWithNRoot extends FieldWithNRoot[Float]
                                        with FloatIsNRoot with FloatIsField
  implicit object RealIsFieldWithNRoot extends FieldWithNRoot[Real]
                                       with RealIsNRoot with RealIsField
  implicit object BigDecimalIsFieldWithNRoot extends FieldWithNRoot[BigDecimal]
                                  with BigDecimalIsNRoot with BigDecimalIsField

  implicit def rationalIsFieldWithNRoot(implicit c: ApproximationContext[Rational]): FieldWithNRoot[Rational] =
    RationalIsFieldWithNRoot(c)

  def apply[A](implicit e: FieldWithNRoot[A]): FieldWithNRoot[A] = e
}


final class NRootOps[@spec(Double, Float, Int, Long) A](lhs: A)(implicit n: NRoot[A]) {
  def nroot(k: Int): A = n.nroot(lhs, k)
  def sqrt: A = n.sqrt(lhs)
  def log = n.log(lhs)
  def ***(rhs:A) = n.fpow(lhs, rhs)
}


trait DoubleIsNRoot extends NRoot[Double] {
  def nroot(a: Double, k: Int): Double = mth.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = mth.sqrt(a)
  def log(a:Double) = scala.math.log(a)
  def fpow(a:Double, b:Double) = scala.math.pow(a, b)
}

trait FloatIsNRoot extends NRoot[Float] {
  def nroot(a: Float, k: Int): Float = mth.pow(a, 1 / k.toDouble).toFloat
  override def sqrt(a: Float): Float = mth.sqrt(a).toFloat
  def log(a:Float) = scala.math.log(a).toFloat
  def fpow(a:Float, b:Float) = scala.math.pow(a, b).toFloat
}


trait RationalIsNRoot extends NRoot[Rational] {
  implicit def context: ApproximationContext[Rational]
  def nroot(a: Rational, k: Int): Rational = a nroot k
  def log(a:Rational) = a.log
  def fpow(a:Rational, b:Rational) = a.pow(b)
}

case class RationalIsFieldWithNRoot(context: ApproximationContext[Rational])
extends FieldWithNRoot[Rational] with RationalIsNRoot with RationalIsField


trait RealIsNRoot extends NRoot[Real] {
  def nroot(a: Real, k: Int): Real = a nroot k
  def log(a:Real) = sys.error("fixme")
  def fpow(a:Real, b:Real) = sys.error("fixme")
}

trait ComplexIsNRoot[@spec(Float,Double) A] extends NRoot[Complex[A]] {
  implicit val f:Fractional[A]
  def log(a:Complex[A]) = a.log
  def fpow(a:Complex[A], b:Complex[A]) = a.pow(b)
}

class ComplexIsNRootCls[@spec(Float,Double) A]
(implicit val f:Fractional[A], val t:Trig[A]) extends ComplexIsNRoot[A] {
  def nroot(a:Complex[A], k:Int) = a.pow(Complex.one[A] / Complex(f.fromInt(k), f.zero))
}


trait BigDecimalIsNRoot extends NRoot[BigDecimal] {
  def nroot(a: BigDecimal, k: Int): BigDecimal = if (a.mc.getPrecision <= 0) {
    throw new ArithmeticException("Cannot find the nroot of a BigDecimal with unlimited precision.")
  } else {
    NRoot.nroot(a, k, a.mc)
  }
  def log(a:BigDecimal) = spire.math.fun.log(a)
  def fpow(a:BigDecimal, b:BigDecimal) = fun.pow(a, b)
}


trait IntIsNRoot extends NRoot[Int] { self: Ring[Int] =>
  def nroot(x: Int, n: Int): Int = {
    def findnroot(prev: Int, add: Int): Int = {
      val next = prev | add
      val e = self.pow(next, n)

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
  def log(a:Int) = scala.math.log(a.toDouble).toInt
  def fpow(a:Int, b:Int) = pow(a, b)
}

trait LongIsNRoot extends NRoot[Long] { self: Ring[Long] =>
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
      val next = prev | add
      val e = self.pow(next, n)

      if (e == x) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1L << ((65 - n) / n))
  }
  def log(a:Long) = scala.math.log(a.toDouble).toLong
  def fpow(a:Long, b:Long) = fun.pow(a, b)
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
  def log(a:BigInt) = Numeric[BigDecimal].log(BigDecimal(a)).toBigInt
  def fpow(a:BigInt, b:BigInt) = fun.pow(BigDecimal(a), BigDecimal(b)).toBigInt
}



object NRoot {

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


