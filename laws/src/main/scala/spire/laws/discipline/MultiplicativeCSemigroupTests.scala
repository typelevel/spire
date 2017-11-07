package spire.laws.discipline

import spire.algebra.{MultiplicativeCSemigroup, Eq}
import spire.laws.MultiplicativeCSemigroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MultiplicativeCSemigroupTests[A] extends MultiplicativeSemigroupTests[A] with CommutativeTests {

  def laws: MultiplicativeCSemigroupLaws[A]

  def multiplicativeCSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "multiplicativeCSemigroup",
      multiplicativeSemigroup,
      None,
      "times commutative" -> forAll(laws.timesCommutative _)
    )
}

object MultiplicativeCSemigroupTests {
  def apply[A: MultiplicativeCSemigroup]: MultiplicativeCSemigroupTests[A] =
    new MultiplicativeCSemigroupTests[A] { def laws: MultiplicativeCSemigroupLaws[A] = MultiplicativeCSemigroupLaws[A] }
}
