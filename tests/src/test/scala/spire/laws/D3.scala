package spire.laws

import spire.algebra._

import org.scalacheck.{Arbitrary, Gen}

/**
 * The smallest non-abelian group, for testing non-abelian groups.
 */
final case class D3(n: Int, m: Boolean)

object D3 {
  implicit object D3Algebra extends Group[D3] with Eq[D3] {
    def id: D3 = D3(0, false)

    def op(x: D3, y: D3): D3 = {
      val D3(n1, m) = y
      x match {
        case D3(n0, false) => D3((n0 + n1) % 3, m)
        case D3(n0, true) => D3((n0 + 3 - n1) % 3, !m)
      }
    }

    def inverse(x: D3): D3 = x match {
      case D3(_, true) => x
      case D3(n, false) => D3((3 - n) % 3, false)
    }

    def eqv(x: D3, y: D3): Boolean = x == y
  }

  implicit def D3Aribtrary: Arbitrary[D3] = 
    Arbitrary(Gen.oneOf(0, 1, 2, 3, 4, 5).map { x =>
      D3(x / 2, x % 2 == 0)
    })
}
