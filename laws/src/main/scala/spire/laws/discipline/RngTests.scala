package spire.laws
package discipline

import spire.algebra.{Rng, Eq}

import org.scalacheck.Arbitrary

trait RngTests[A] extends SemiringTests[A] with AdditiveAbGroupTests[A] {
  def laws: RngLaws[A]

  def rng(implicit arbA: Arbitrary[A], eqA: Eq[A]): RingRuleSet =
    new RingRuleSet(
      "rng",
      additiveAbGroup,
      multiplicativeSemigroup,
      Seq(semiring)
    )
}

object RngTests {
  def apply[A: Rng]: RngTests[A] =
    new RngTests[A] { def laws: RngLaws[A] = RngLaws[A] }
}
