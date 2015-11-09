package spire
package laws

import spire.algebra._

import org.scalacheck.{Arbitrary, Gen}

/**
 * The smallest non-abelian group, for testing non-abelian groups.
 */
final class D3 private (val n: Int) extends AnyVal

object D3 {
  final def apply(n: Int): D3 = new D3(n % 6)

  implicit object D3Algebra extends Group[D3] with Eq[D3] {
    def id: D3 = new D3(0)

    def op(x: D3, y: D3): D3 = {
      val n0 = x.n / 2
      val n1 = y.n / 2
      val m = y.n % 2

      if (x.n % 2 == 0) {
        new D3(((n0 + n1) % 3) * 2 + m)
      } else {
        new D3(((n0 + 3 - n1) % 3) * 2 + (m ^ 1))
      }
    }

    def inverse(x: D3): D3 =
      if (x.n % 2 == 1) x else new D3(((3 - x.n / 2) % 3) * 2)

    def eqv(x: D3, y: D3): Boolean = x.n == y.n
  }

  implicit def D3Aribtrary: Arbitrary[D3] =
    Arbitrary(Gen.oneOf(0, 1, 2, 3, 4, 5).map(D3(_)))
}
