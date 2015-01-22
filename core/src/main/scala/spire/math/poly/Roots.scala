package spire.math
package poly

import java.math.{ RoundingMode, MathContext }

import spire.std.bigInt._
import spire.std.bigDecimal._

/**
 * A trait that can be used to retreive the (possibly approximated) real
 * roots of the polynomial `poly`.
 */
trait Roots[A] { self =>

  /** The polynomial the roots belong to. */
  def poly: Polynomial[A]

  /** Returns the number of real roots of `poly`. */
  def count: Int

  /**
   * Returns the `i`-th real root of `poly`, or throws an
   * `IndexOutOfBoundsException` if there is no `i`-th real root.
   */
  def get(i: Int): A
}

object Roots {
  final def isolateRoots[A](poly: Polynomial[A])(implicit isolator: RootIsolator[A]): Vector[Interval[Rational]] =
    isolator.isolateRoots(poly)

  /**
   * Returns a polynomial with the same roots as `poly`, but only integer coefficients.
   */
  final def removeFractions(poly: Polynomial[Rational]): Polynomial[BigInt] = {
    val coeffs = poly.coeffsArray
    val factors = coeffs.foldLeft(BigInt(1)) { (acc, coeff) =>
      val d = coeff.denominator
      acc * (d / acc.gcd(d))
    }
    val zCoeffs = coeffs.map(n => n.numerator * (factors / n.denominator))
    Polynomial.dense(zCoeffs)
  }

  /**
   * Returns a polynomial with the same roots as `poly`, but only integer coefficients.
   */
  final def removeDecimal(poly: Polynomial[BigDecimal]): Polynomial[BigInt] = {
    if (poly == Polynomial.zero[BigDecimal]) {
      Polynomial.zero[BigInt]
    } else {
      val terms = poly.terms.map { case Term(c, e) =>
        Term(c.bigDecimal.stripTrailingZeros, e)
      }
      val maxScale = terms.map(_.coeff.scale).max
      Polynomial(terms.map { case Term(c, e) =>
        val c0 = BigInt(c.movePointRight(maxScale).unscaledValue)
        Term(c0, e)
      })
    }
  }

  /**
   * Returns an upper bit bound on the roots of the polynomial `p`.
   */
  final def upperBound(p: Polynomial[BigInt]): Int = {
    val lgLastCoeff = p.maxOrderTermCoeff.abs.bitLength
    val n = p.degree
    var maxBound = Double.NegativeInfinity
    p.foreachNonZero { (k, coeff) =>
      if (k != n) {
        val i = n - k
        val bound = ((coeff.abs.bitLength - lgLastCoeff - 1) / i) + 2
        maxBound = max(maxBound, bound.toDouble)
      }
    }
    if (maxBound.isValidInt) {
      maxBound.toInt
    } else {
      throw new ArithmeticException("bound too large")
    }
  }

  /**
   * Returns an lower bit bound on the roots of the polynomial `p`.
   */
  def lowerBound(p: Polynomial[BigInt]): Int =
    -upperBound(p.reciprocal)
}

private[poly] class BigDecimalSimpleRoots(
  val poly: Polynomial[BigDecimal],
  scale: Int
) extends Roots[BigDecimal] {
  private val zpoly: Polynomial[BigInt] = Roots.removeDecimal(poly)
  private val isolated: Vector[Interval[Rational]] = Roots.isolateRoots(zpoly)

  def count: Int = isolated.size

  def get(i: Int): BigDecimal = if (i < 0 || i >= count) {
    throw new IndexOutOfBoundsException(i.toString)
  } else {
    isolated(i) match {
      case Point(value) =>
        value.toBigDecimal(scale, RoundingMode.HALF_EVEN)
      case Bounded(lb, ub, _) =>
        new BigDecimal(
          BigDecimalRootRefinement(zpoly, lb, ub, scale).approximation,
          MathContext.UNLIMITED
        )
      case _ =>
        throw new RuntimeException("invalid isolated root interval")
    }
  }
}

private[poly] class BigDecimalRelativeRoots(
  val poly: Polynomial[BigDecimal],
  mc: MathContext
) extends Roots[BigDecimal] {
  private val zpoly: Polynomial[BigInt] = Roots.removeDecimal(poly)
  private val isolated: Vector[Interval[Rational]] = Roots.isolateRoots(zpoly)

  def count: Int = isolated.size

  def get(i: Int): BigDecimal = if (i < 0 || i >= count) {
    throw new IndexOutOfBoundsException(i.toString)
  } else {
    isolated(i) match {
      case Point(value) =>
        value.toBigDecimal(mc)
      case Bounded(lb, ub, _) =>
        Algebraic.unsafeRoot(zpoly, lb, ub).toBigDecimal(mc)
      case _ =>
        throw new RuntimeException("invalid isolated root interval")
    }
  }
}
