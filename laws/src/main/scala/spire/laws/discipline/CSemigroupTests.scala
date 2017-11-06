package spire.laws.discipline

import spire.algebra.{CSemigroup, Eq}
import spire.laws.CSemigroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait CSemigroupTests[A] extends SemigroupTests[A] {

  def laws: CSemigroupLaws[A]

  def CSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "CSemigroup",
      Some(Semigroup),
      "combine commutative" -> forAll(laws.combineCommutative _)
    )
}

object CSemigroupTests {
  def apply[A: CSemigroup]: CSemigroupTests[A] =
    new CSemigroupTests[A] { def laws: CSemigroupLaws[A] = CSemigroupLaws[A] }
}
