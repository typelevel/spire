package spire.laws
package discipline

import spire.algebra.{CRng, Eq}

import org.scalacheck.Arbitrary

trait CRngTests[A] extends RngTests[A] with CSemiringTests[A] {
  def laws: CRngLaws[A]

  def cRng(implicit arbA: Arbitrary[A], eqA: Eq[A]): CRingRuleSet =
    new CRingRuleSet(
      "cRng",
      rng,
      Seq(cSemiring)
    )
}

object CRngTests {
  def apply[A: CRng]: CRngTests[A] =
    new CRngTests[A] { def laws: CRngLaws[A] = CRngLaws[A] }
}
