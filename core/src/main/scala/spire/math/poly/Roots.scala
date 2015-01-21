package spire.math
package poly

import java.math.{ RoundingMode, MathContext }

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

private[poly] class BigDecimalRoots(
  val poly: Polynomial[BigDecimal],
  scale: Int
) extends Roots[BigDecimal] {
  private val qPoly: Polynomial[Rational] = poly.map(Rational(_))
  private val isolated: Vector[Interval[Rational]] = Polynomial.isolateRoots(qPoly)

  def count: Int = isolated.size

  def get(i: Int): BigDecimal = if (i < 0 || i >= count) {
    throw new IndexOutOfBoundsException(i.toString)
  } else {
    isolated(i) match {
      case Point(value) =>
        value.toBigDecimal(scale, RoundingMode.HALF_EVEN)
      case Bounded(lb, ub, _) =>
        new BigDecimal(
          BigDecimalRootRefinement.QIR(qPoly, lb, ub, scale).approximation,
          MathContext.UNLIMITED
        )
      case _ =>
        throw new RuntimeException("invalid isolated root interval")
    }
  }
}
