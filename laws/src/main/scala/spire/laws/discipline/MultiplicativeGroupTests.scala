package spire.laws.discipline

import spire.algebra.{MultiplicativeGroup, Eq}
import spire.laws.MultiplicativeGroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MultiplicativeGroupTests[A] extends MultiplicativeMonoidTests[A] {

  def laws: MultiplicativeGroupLaws[A]

  def multiplicativeGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "multiplicativeGroup",
      Some(multiplicativeMonoid),
      "left reciprocal" -> forAll(laws.leftReciprocal _),
      "right reciprocal" -> forAll(laws.rightReciprocal _),
      "consistent div" -> forAll(laws.consistentDiv _)
    )
}

object MultiplicativeGroupTests {
  def apply[A: MultiplicativeGroup]: MultiplicativeGroupTests[A] =
    new MultiplicativeGroupTests[A] { def laws: MultiplicativeGroupLaws[A] = MultiplicativeGroupLaws[A] }
}
