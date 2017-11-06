package spire.laws.discipline

import spire.algebra.{MultiplicativeCSemigroup, Eq}
import spire.laws.MultiplicativeCSemigroupLaws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MultiplicativeCSemigroupTests[A] extends MultiplicativeSemigroupTests[A] {

  def laws: MultiplicativeCSemigroupLaws[A]

  def MultiplicativeCSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "MultiplicativeCSemigroup",
      Some(MultiplicativeSemigroup),
      "times commutative" -> forAll(laws.timesCommutative _)
    )
}

object MultiplicativeCSemigroupTests {
  def apply[A: MultiplicativeCSemigroup]: MultiplicativeCSemigroupTests[A] =
    new MultiplicativeCSemigroupTests[A] { def laws: MultiplicativeCSemigroupLaws[A] = MultiplicativeCSemigroupLaws[A] }
}
