package spire.laws.discipline

import spire.algebra.{AdditiveCSemigroup, Eq}
import spire.laws.AdditiveCSemigroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait AdditiveCSemigroupTests[A] extends AdditiveSemigroupTests[A] {

  def laws: AdditiveCSemigroupLaws[A]

  def additiveCSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "additiveCSemigroup",
      Some(additiveSemigroup),
      "plus commutative" -> forAll(laws.plusCommutative _)
    )
}

object AdditiveCSemigroupTests {
  def apply[A: AdditiveCSemigroup]: AdditiveCSemigroupTests[A] =
    new AdditiveCSemigroupTests[A] { def laws: AdditiveCSemigroupLaws[A] = AdditiveCSemigroupLaws[A] }
}
