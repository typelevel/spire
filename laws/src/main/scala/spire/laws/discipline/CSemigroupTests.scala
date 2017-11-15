package spire.laws.discipline

import spire.algebra.{CSemigroup, Eq}
import spire.laws.CSemigroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait CSemigroupTests[A] extends SemigroupTests[A] with CommutativeTests {

  def laws: CSemigroupLaws[A]

  def cSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "cSemigroup",
      semigroup,
      None,
      "combine commutative" -> forAll(laws.combineCommutative _)
    )
}

object CSemigroupTests {
  def apply[A: CSemigroup]: CSemigroupTests[A] =
    new CSemigroupTests[A] { def laws: CSemigroupLaws[A] = CSemigroupLaws[A] }
}
