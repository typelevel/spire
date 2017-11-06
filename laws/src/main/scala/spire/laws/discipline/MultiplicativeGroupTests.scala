package spire.laws.discipline

import spire.algebra.{MultiplicativeGroup, Eq}
import spire.laws.MultiplicativeGroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MultiplicativeGroupTests[A] extends MultiplicativeMonoidTests[A] {

  def laws: MultiplicativeGroupLaws[A]

  def MultiplicativeGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "MultiplicativeGroup",
      Some(MultiplicativeMonoid),
      "left negate" -> forAll(laws.leftNegate _),
      "right negate" -> forAll(laws.rightNegate _),
      "consistent minus" -> forAll(laws.consistentMinus _)
    )
}

object MultiplicativeGroupTests {
  def apply[A: MultiplicativeGroup]: MultiplicativeGroupTests[A] =
    new MultiplicativeGroupTests[A] { def laws: MultiplicativeGroupLaws[A] = MultiplicativeGroupLaws[A] }
}
