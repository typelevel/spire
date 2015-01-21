package spire.math
package poly

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
  implicit def BigDecimalScaleRootFinder(scale: Int): RootFinder[BigDecimal] =
    new RootFinder[BigDecimal] {
      def findRoots(poly: Polynomial[BigDecimal]): Roots[BigDecimal] =
        new BigDecimalRoots(poly, scale)
    }
}
