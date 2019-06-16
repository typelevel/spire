package spire
package laws

import spire.algebra._
import spire.implicits._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import InvalidTestException._
import org.typelevel.discipline.Laws

object CombinationLaws {
  def apply[A : Eq : Arbitrary] = new CombinationLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

/** Contains laws that are obeying by combination of types, for example
  * various kinds of signed rings.
  */
trait CombinationLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = None,
    "ordered group" -> forAllSafe { (x: A, y: A, z: A) =>
      !(x <= y) || (x + z <= y + z) // replaces (x <= y) ==> (x + z <= y + z)
    },
    "triangle inequality" -> forAllSafe { (x: A, y: A) =>
      (x + y).abs <= x.abs + y.abs
    }
  )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCMonoid),
    "abs(x) equals abs(-x)" -> forAllSafe { (x: A) =>
      x.abs === (-x).abs
    }
  )

  // more a convention: as GCD is defined up to a unit, so up to a sign,
  // on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  // behavior of computer algebra systems
  def signedGCDRing(implicit signedA: Signed[A], gcdRingA: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveAbGroup),
    "gcd(x, y) >= 0" -> forAllSafe { (x: A, y: A) =>
      x.gcd(y).signum >= 0
    },
    "gcd(x, 0) === abs(x)" -> forAllSafe { (x: A) =>
      x.gcd(Ring[A].zero) === Signed[A].abs(x)
    }
  )

}

// vim: expandtab:ts=2:sw=2
