package spire.laws
package discipline

import spire.algebra.{Rig, Eq}

import org.scalacheck.Arbitrary

trait RigTests[A] extends SemiringTests[A] with MultiplicativeMonoidTests[A] {
  def laws: RigLaws[A]

  def rig(implicit arbA: Arbitrary[A], eqA: Eq[A]): RingRuleSet =
    new RingRuleSet(
      "rig",
      additiveCMonoid,
      multiplicativeMonoid,
      Seq(semiring)
    )
}

object RigTests {
  def apply[A: Rig]: RigTests[A] =
    new RigTests[A] { def laws: RigLaws[A] = RigLaws[A] }
}
