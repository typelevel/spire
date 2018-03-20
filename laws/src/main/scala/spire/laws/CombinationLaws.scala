package spire
package laws

import spire.algebra._
import spire.implicits._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object CombinationLaws {
  def apply[A : Eq : Arbitrary] = new CombinationLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

/** Contains laws that are obeying by combination of types, for example
  * various kinds of signed rings. 
  */
trait CombinationLaws[A] extends BaseLaws[A] {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = None,
    "ordered group" → forAll { (x: A, y: A, z: A) =>
      (x <= y) ==> (x + z <= y + z)
    },
    "triangle inequality" → forAll { (x: A, y: A) =>
      (x + y).abs <= x.abs + y.abs
    }
  )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCMonoid),
    "abs(x) equals abs(-x)" → forAll { (x: A) =>
      x.abs === (-x).abs
    }
  )

  // more a convention: as GCD is defined up to a unit, so up to a sign,
  // on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  // behavior of computer algebra systems
  def signedGCDRing(implicit signedA: Signed[A], gcdRingA: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveAbGroup),
    "gcd(x, y) >= 0" → forAll { (x: A, y: A) =>
      x.gcd(y).signum >= 0
    },
    "gcd(x, 0) === abs(x)" → forAll { (x: A) =>
      x.gcd(Ring[A].zero) === Signed[A].abs(x)
    }
  )

}

// vim: expandtab:ts=2:sw=2
