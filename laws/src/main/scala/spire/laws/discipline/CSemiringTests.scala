package spire.laws
package discipline

import spire.algebra.{CSemiring, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait CSemiringTests[A] extends SemiringTests[A] with MultiplicativeCSemigroupTests[A] {
  def laws: CSemiringLaws[A]

  def cSemiring(implicit arbA: Arbitrary[A], eqA: Eq[A]): CRingRuleSet =
    new CRingRuleSet(
      "cSemiring",
      semiring,
      Nil
    )

  class CRingRuleSet(val name: String,
    val noncommutativeParent: RingRuleSet,
    val commutativeParents: Seq[CRingRuleSet],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Nil
    val parents = Seq(noncommutativeParent) ++ commutativeParents
  }
}

object CSemiringTests {
  def apply[A: CSemiring]: CSemiringTests[A] =
    new CSemiringTests[A] { def laws: CSemiringLaws[A] = CSemiringLaws[A] }
}
