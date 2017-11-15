package spire.laws
package discipline

import spire.algebra.{CRing, Eq}

import org.scalacheck.Arbitrary

trait CRingTests[A] extends RingTests[A] with CRigTests[A] with CRngTests[A] {
  def laws: CRingLaws[A]

  def cRing(implicit arbA: Arbitrary[A], eqA: Eq[A]): CRingRuleSet =
    new CRingRuleSet(
      "cRing",
      rng,
      Seq(cRig, cRng)
    )
}

object CRingTests {
  def apply[A: CRing]: CRingTests[A] =
    new CRingTests[A] { def laws: CRingLaws[A] = CRingLaws[A] }
}
