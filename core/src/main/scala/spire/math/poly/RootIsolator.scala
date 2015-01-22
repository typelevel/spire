package spire.math
package poly

import spire.std.bigInt._
import spire.std.bigDecimal._

/**
 * A type class for retreiving isolated roots.
 */
sealed trait RootIsolator[A] {

  /**
   * Isolates the roots of the [[Rational]] polynomial `poly`. This returns a
   * sequence of intervals that each contain a single root of `poly`. A root
   * will appear in the sequence as many times as its multiplicity in the
   * polynomial. Other than this, all root intervals are disjoint and are
   * either open on both ends or is a single point.
   */
  def isolateRoots(poly: Polynomial[A]): Vector[Interval[Rational]]
}

object RootIsolator {

  implicit val RationalRootIsolator: RootIsolator[Rational] = new RootIsolator[Rational] {
    def isolateRoots(poly: Polynomial[Rational]): Vector[Interval[Rational]] =
      VAS(Roots.removeFractions(poly))
  }

  implicit val BigDecimalRootIsolator: RootIsolator[BigDecimal] = new RootIsolator[BigDecimal] {
    def isolateRoots(poly: Polynomial[BigDecimal]): Vector[Interval[Rational]] =
      VAS(Roots.removeDecimal(poly))
  }

  implicit val BigIntRootIsolator: RootIsolator[BigInt] = new RootIsolator[BigInt] {
    def isolateRoots(poly: Polynomial[BigInt]): Vector[Interval[Rational]] =
      VAS(poly)
  }

  /**
   * An implementation of the VAS real root isolation algorithm.
   *
   * See "A Comparative Study of Two Real Root Isolation Methods" for the paper
   * that originally presented the method implemented here, and "Complexity
   * Analysis of Algorithms in Algebraic Computation" by Vikram Sharma which
   * goes into greater detail.
   */
  private final def VAS(poly: Polynomial[BigInt]): Vector[Interval[Rational]] = {
    val x = Polynomial.x[BigInt]
    val one = Polynomial.one[BigInt]

    // Find all roots recursively that are between (0, 1) and (1, infinity).
    def split1(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): Vector[Interval[Rational]] = {
      val r = p.compose(x + one)
      val rRoots = rec(r, a, b + a, c, d + c)
      if (r.signVariations < p.signVariations) {
        var l = p.reciprocal.compose(x + one)
        while (l(0) == 0)
          l = l.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
        val lRoots = rec(l, b, a + b, d, c + d)
        lRoots ++ rRoots
      } else {
        rRoots
      }
    }

    // Isolate all positive roots in polynomial p.
    def rec(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): Vector[Interval[Rational]] = {
      if (p(BigInt(0)) == BigInt(0)) {
        val p0 = p.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
        Interval.point(Rational(b, d)) +: rec(p0, a, b, c, d)
      } else {
        p.signVariations match {
          case 0 => // No roots.
            Vector.empty

          case 1 => // Isolated exactly 1 root.
            def ub = {
              val exp = Roots.upperBound(p)
              if (exp >= 0) Rational(BigInt(1) << exp)
              else Rational(1, BigInt(1) << -exp)
            }
            val i0 = if (c == 0) ub else Rational(a, c)
            val i1 = if (d == 0) ub else Rational(b, d)
            if (i0 < i1) Vector(Interval.open(i0, i1))
            else Vector(Interval.open(i1, i0))

          case _ => // Exists 0 or 2 or more roots.
            val lb = Roots.lowerBound(p)
            if (lb < 0) {
              split1(p, a, b, c, d)
            } else {
              val flr = BigInt(1) << lb
              split1(p.compose(x + Polynomial.constant(flr)), a, b + a * flr, c, d + c * flr)
            }
        }
      }
    }

    val zeroInterval = Interval.point(Rational.zero)
    val posRoots = rec(poly, 1, 0, 0, 1)
    val negRoots = rec(poly.flip, 1, 0, 0, 1).map(-_).filter(_ != zeroInterval)
    negRoots ++ posRoots
  }
}
