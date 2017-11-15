package spire.laws
package discipline

import spire.algebra.{CRig, Eq}

import org.scalacheck.Arbitrary

trait CRigTests[A] extends RigTests[A] with CSemiringTests[A] {
  def laws: CRigLaws[A]

  def cRig(implicit arbA: Arbitrary[A], eqA: Eq[A]): CRingRuleSet =
    new CRingRuleSet(
      "cRig",
      rig,
      Seq(cSemiring)
    )
}

object CRigTests {
  def apply[A: CRig]: CRigTests[A] =
    new CRigTests[A] { def laws: CRigLaws[A] = CRigLaws[A] }
}
