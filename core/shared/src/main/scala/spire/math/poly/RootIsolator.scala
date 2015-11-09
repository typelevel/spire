package spire
package math
package poly

import spire.std.bigInt._

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

    case class TransformedPoly(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt)

    // Find all roots recursively that are between (0, 1) and (1, infinity).
    def split1(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): List[TransformedPoly] = {
      val r = p.compose(x + one)
      val rRoots = TransformedPoly(r, a, b + a, c, d + c)
      if (r.signVariations < p.signVariations) {
        var l = p.reciprocal.compose(x + one)
        while (l(0) == 0)
          l = l.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
        val lRoots = TransformedPoly(l, b, a + b, d, c + d)
        lRoots :: rRoots :: Nil
      } else {
        rRoots :: Nil
      }
    }

    // Isolate all positive roots in polynomial p.
    def rec(polys: List[TransformedPoly], acc: Vector[Interval[Rational]] = Vector.empty): Vector[Interval[Rational]] = polys match {
      case TransformedPoly(p, a, b, c, d) :: rest =>
        if (p(BigInt(0)) == BigInt(0)) {
          val p0 = p.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
          rec(TransformedPoly(p0, a, b, c, d) :: rest, acc :+ Interval.point(Rational(b, d)))
        } else {
          p.signVariations match {
            case 0 => // No roots.
              rec(rest, acc)

            case 1 => // Isolated exactly 1 root.
              def ub: Rational = {
                val exp = Roots.upperBound(p)
                // This is an upper bound for p, but not for the initial poly.
                val ub0 =
                  if (exp >= 0) Rational(BigInt(1) << exp)
                  else Rational(1, BigInt(1) << -exp)
                // We map the upper bound for p back to a bound for the initial
                // polynomial by using the inverse Mobius transformation.
                (Rational(d) * ub0 - Rational(b)) / (Rational(-c) * ub0 + Rational(a))
              }
              val i0 = if (c == 0) ub else Rational(a, c)
              val i1 = if (d == 0) ub else Rational(b, d)
              if (i0 < i1) rec(rest, acc :+ Interval.open(i0, i1))
              else rec(rest, acc :+ Interval.open(i1, i0))

            case _ => // Exists 0 or 2 or more roots.
              val lb = Roots.lowerBound(p)
              if (lb < 0) {
                val more = split1(p, a, b, c, d)
                rec(more reverse_::: rest, acc)
              } else {
                val flr = BigInt(1) << lb
                val more = split1(p.compose(x + Polynomial.constant(flr)), a, b + a * flr, c, d + c * flr)
                rec(more reverse_::: rest, acc)
              }
          }
        }

      case Nil =>
        acc
    }

    if (poly.isConstant) {
      // A degenerate case we cannot handle.
      Vector.empty
    } else {
      val zeroInterval = Interval.point(Rational.zero)
      val posRoots = rec(TransformedPoly(poly, 1, 0, 0, 1) :: Nil)
      val negRoots = rec(TransformedPoly(poly.flip, 1, 0, 0, 1) :: Nil).map(-_).filter(_ != zeroInterval)
      negRoots ++ posRoots
    }
  }
}
