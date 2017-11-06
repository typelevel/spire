package spire.laws.discipline

import spire.algebra.{AdditiveGroup, Eq}
import spire.laws.AdditiveGroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait AdditiveGroupTests[A] extends AdditiveMonoidTests[A] {

  def laws: AdditiveGroupLaws[A]

  def additiveGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "additiveGroup",
      Some(additiveMonoid),
      "left negate" -> forAll(laws.leftNegate _),
      "right negate" -> forAll(laws.rightNegate _),
      "consistent minus" -> forAll(laws.consistentMinus _)
    )
}

object AdditiveGroupTests {
  def apply[A: AdditiveGroup]: AdditiveGroupTests[A] =
    new AdditiveGroupTests[A] { def laws: AdditiveGroupLaws[A] = AdditiveGroupLaws[A] }
}
