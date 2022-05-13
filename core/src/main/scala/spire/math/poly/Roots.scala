/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math
package poly

import java.math.{MathContext, RoundingMode}

import spire.std.bigInt._
import spire.std.bigDecimal._

/**
 * A trait that can be used to retrieve the (possibly approximated) real roots of the polynomial `poly`.
 */
trait Roots[A] extends Iterable[A] { self =>

  /**
   * The polynomial the roots belong to.
   */
  def poly: Polynomial[A]

  /**
   * Returns the number of real roots of `poly`.
   */
  def count: Int

  /**
   * Returns the `i`-th real root of `poly`, or throws an `IndexOutOfBoundsException` if there is no `i`-th real root.
   */
  def get(i: Int): A

  def iterator: Iterator[A] = Iterator.tabulate(count)(get)

  override def size: Int = count

  override def toString: String =
    mkString("Roots(", ", ", ")")
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
      val d = coeff.denominator.toBigInt
      acc * (d / acc.gcd(d))
    }
    val zCoeffs = coeffs.map(n => (n.numerator * (factors / n.denominator)).toBigInt)
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
    // We can construct an upper bound on the real roots of p, a polynomial of
    // degree n, by computing a bound for each coefficient, except a_n, as
    // `b_i = nroot(abs(a_i / a_n), n - i) + 1`. We then choose the maximum
    // bound as the bound to return. However, since we're dealing with a pretty
    // loose bound, we can actually skip the n-roots and division and work with
    // bit-bounds instead, which let us replace division with subtraction and
    // n-roots with division.
    val lgLastCoeff = p.maxOrderTermCoeff.abs.bitLength
    val n = p.degree
    var maxBound = Double.NegativeInfinity
    p.foreachNonZero { (k, coeff) =>
      if (k != n) {
        val i = n - k
        // Note: This corresponds to nroot(abs(a_i / a_n), n - 1) + 1, but we
        // add 2 bits to account for the floor division and the +1 at the end.
        val bound = (coeff.abs.bitLength - lgLastCoeff + 1) / i + 2
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
          BigDecimalRootRefinement(poly, lb, ub, scale).approximateValue,
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
        Algebraic.unsafeRoot(zpoly, i, lb, ub).toBigDecimal(mc)
      case _ =>
        throw new RuntimeException("invalid isolated root interval")
    }
  }
}

// FIXME: This is pretty hacky. We should implement proper exact real roots:
// http://arxiv.org/pdf/1011.0344v2.pdf
// http://arxiv.org/pdf/1104.1362v3.pdf
private[poly] class FixedRealRoots(
  val poly: Polynomial[Real]
) extends Roots[Real] {
  private val zpoly: Polynomial[BigInt] = Roots.removeFractions(poly.map(_.toRational))
  private val isolated: Vector[Interval[Rational]] = Roots.isolateRoots(zpoly)

  def count: Int = isolated.size

  def get(i: Int): Real = if (i < 0 || i >= count) {
    throw new IndexOutOfBoundsException(i.toString)
  } else {
    isolated(i) match {
      case Point(value) =>
        Real(value)
      case Bounded(lb, ub, _) =>
        Real(
          Algebraic
            .unsafeRoot(zpoly, i, lb, ub)
            .toBigDecimal(new MathContext(Real.digits, RoundingMode.HALF_EVEN))
        )
      case _ =>
        throw new RuntimeException("invalid isolated root interval")
    }
  }
}

private[poly] class NumberRoots(
  val poly: Polynomial[Number]
) extends Roots[Number] {
  private val roots = new BigDecimalRelativeRoots(poly.map(_.toBigDecimal), BigDecimal.defaultMathContext)

  def count: Int = roots.count

  def get(i: Int): Number = Number(roots.get(i))
}
