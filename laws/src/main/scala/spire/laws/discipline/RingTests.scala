package spire.laws
package discipline

import spire.algebra.{Ring, Eq}

import org.scalacheck.Arbitrary

trait RingTests[A] extends RigTests[A] with RngTests[A] {
  def laws: RingLaws[A]

  def ring(implicit arbA: Arbitrary[A], eqA: Eq[A]): RingRuleSet =
    new RingRuleSet(
      "ring",
      additiveAbGroup,
      multiplicativeMonoid,
      Seq(rig, rng)
    )
}

object RingTests {
  def apply[A: Ring]: RingTests[A] =
    new RingTests[A] { def laws: RingLaws[A] = RingLaws[A] }
}
