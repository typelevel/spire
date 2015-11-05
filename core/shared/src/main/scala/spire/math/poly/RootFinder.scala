package spire
package math
package poly

import java.math.MathContext

/**
 * A type class that can find roots of a polynomial.
 */
trait RootFinder[A] {

  /**
   * Returns the roots of the polynomial `poly`.
   */
  def findRoots(poly: Polynomial[A]): Roots[A]
}

object RootFinder {
  final def apply[A](implicit finder: RootFinder[A]): RootFinder[A] = finder

  implicit def BigDecimalScaleRootFinder(scale: Int): RootFinder[BigDecimal] =
    new RootFinder[BigDecimal] {
      def findRoots(poly: Polynomial[BigDecimal]): Roots[BigDecimal] =
        new BigDecimalSimpleRoots(poly, scale)
    }

  implicit def BigDecimalMathContextRootFinder(mc: MathContext): RootFinder[BigDecimal] =
    new RootFinder[BigDecimal] {
      def findRoots(poly: Polynomial[BigDecimal]): Roots[BigDecimal] =
        new BigDecimalRelativeRoots(poly, mc)
    }

  implicit val RealRootFinder: RootFinder[Real] =
    new RootFinder[Real] {
      def findRoots(p: Polynomial[Real]): Roots[Real] =
        new FixedRealRoots(p)
    }

  implicit val NumberRootFinder: RootFinder[Number] =
    new RootFinder[Number] {
      def findRoots(p: Polynomial[Number]): Roots[Number] =
        new NumberRoots(p)
    }
}
