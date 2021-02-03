package spire
package math
package poly

import spire.algebra.Order
import spire.optional.intervalGeometricPartialOrder
import spire.std.bigInt._
import spire.syntax.std.seq._

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
  final private def VAS(poly: Polynomial[BigInt]): Vector[Interval[Rational]] = {

    // As we go through the VAS algorithm, we transform the polynomial by
    // shifting it along the x-axis, flipping it about the y-axis, or inverting
    // it (polynomial reciprocal). When we isolate a root in a transformed
    // polynomial, we need to be able to convert the interval that contains the
    // transformed poly's root into an interval that contains the original
    // polynomial's root. To do this, we also maintain a Mobius transformation
    // (az + b) / (cz + d) that can map points from the transformed space back
    // into the original space.
    case class TransformedPoly(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt)

    // Find all roots recursively that are between (0, 1) and (1, infinity).
    def split1(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): List[TransformedPoly] = {
      // We compose both the polynomial and the Mobius transform with x+1. This
      // polynomial will be shifted 1 to the left, so all our roots between (0,
      // 1) will become negative (and thus ignored in this algo).
      val r = p.shift(BigInt(1))
      val rRoots = TransformedPoly(r, a, b + a, c, d + c)
      if (r.signVariations < p.signVariations) {
        // There may still be roots between (0, 1), so we need to create a
        // polynomial whose positive roots correspond to the roots in (0, 1).
        // We do this by first taking the reciprocal, which maps (0, 1) -> (1,
        // inf), then compose the reciprocal with x+1 to shift (1, inf) to
        // (0, inf). The positive real roots of this polynomial correspond to
        // exactly those roots in (0, 1).
        val l = p.reciprocal.shift(BigInt(1)).removeZeroRoots
        val lRoots = TransformedPoly(l, b, a + b, d, c + d)
        lRoots :: rRoots :: Nil
      } else {
        rRoots :: Nil
      }
    }

    // Isolate all positive roots in polynomial p.
    def rec(polys: List[TransformedPoly], acc: Vector[Interval[Rational]] = Vector.empty): Vector[Interval[Rational]] =
      polys match {
        case TransformedPoly(p, a, b, c, d) :: rest =>
          if (p.nth(0).signum == 0) {
            val p0 = p.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
            rec(TransformedPoly(p0, a, b, c, d) :: rest, acc :+ Interval.point(Rational(b, d)))
          } else {
            p.signVariations match {
              case 0 => // No roots, the base case.
                rec(rest, acc)

              case 1 => // Isolated exactly 1 real, positive root.
                // We found a single positive real root. We use the Mobius
                // transform we've been maintaining to map the points 0 and
                // infinity in the space of the transformed polynomial back to
                // finite points in the space of the original polynomial. We use
                // these points as the boundaries for the (open) interval
                // containing the root.
                //
                // However, a problem is that one of the points may *actually* be
                // infinity and we need finite points. In this case, we can just
                // use an upper bound on the real roots of the polynomial
                // instead. This let's us keep our interval bounded/finite.
                def ub: Rational = {
                  // This is an upper bound for p, but not for the initial poly.
                  val exp = Roots.upperBound(p)
                  val ub0 =
                    if (exp >= 0) Rational(BigInt(1) << exp)
                    else Rational(1, BigInt(1) << -exp)
                  // We map the upper bound for p back to a bound for the initial
                  // polynomial using the Mobius transformation.
                  (Rational(a) * ub0 + Rational(b)) / (Rational(c) * ub0 + Rational(d))
                }
                val i0 = if (c == 0) ub else Rational(a, c) // The point at "inf"
                val i1 = if (d == 0) ub else Rational(b, d) // The point at "0"
                if (i0 < i1) rec(rest, acc :+ Interval.open(i0, i1))
                else rec(rest, acc :+ Interval.open(i1, i0))

              case _ => // Exists 0 or 2 or more roots.
                // In this case we want to split the polynomial into 2 and
                // recursively try both. We do this by splitting the polynomial
                // at (0, 1) and (1, infinity) and recursing (split1). However,
                // if the first root is at , say, x = 1234, then we'd like to
                // avoid checking O(1234) root-less polynomials before we finally
                // find the first root. Luckily, we can easily compute a rough
                // lower bound on the positive real roots of the polynomial. If
                // this is > 1, then we shift the polynomial to the left so the
                // lower bound is now 0. Essentially, we take a shortcut and skip
                // testing these fruitless polynomials.
                //
                // Additionally, since split1 is actually quite expensive, we
                // also take some time to make sure that our lower-bound is
                // pretty tight (see the recursive findFloor below). Turns out
                // this gives us an ~2x perf improvement.

                def findFloor(q: Polynomial[BigInt], floor: Option[BigInt]): List[TransformedPoly] = {
                  val lb = Roots.lowerBound(q)
                  if (lb < 0) {
                    floor.fold(split1(q, a, b, c, d)) { h =>
                      split1(q, a, b + a * h, c, d + c * h)
                    }
                  } else {
                    val h = BigInt(1) << lb
                    val q0 = q.shift(h)
                    if (q0.signVariations == 0) Nil
                    else findFloor(q0, Some(floor.fold(h)(_ + h)))
                  }
                }

                rec(findFloor(p, None).reverse_:::(rest), acc)
            }
          }

        case Nil =>
          acc
      }

    if (poly.isConstant) {
      // A degenerate case we cannot handle.
      Vector.empty
    } else {
      // The algorithm only works on positive roots, so we isolate the positive
      // roots and negative roots separately - the latter by flipping the
      // polynomial about the y-axis.
      val zeroInterval = Interval.point(Rational.zero)
      val posRoots = rec(TransformedPoly(poly, 1, 0, 0, 1) :: Nil)
      val negRoots = rec(TransformedPoly(poly.flip, 1, 0, 0, 1) :: Nil).map(-_).filter(_ != zeroInterval)
      val roots = negRoots ++ posRoots

      // We expect all our isolated roots to be disjoint, so we can use the
      // geometric partial order as a total order on our set of roots to order
      // them correctly.
      val partialOrder = intervalGeometricPartialOrder.intervalGeometricPartialOrder[Rational]
      implicit val order: Order[Interval[Rational]] = Order.from { (x, y) =>
        partialOrder.tryCompare(x, y).getOrElse {
          throw new IllegalStateException("unexpected overlapping isolated roots")
        }
      }
      roots.qsorted
    }
  }
}
