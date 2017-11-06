package spire.laws.discipline

import spire.algebra.{MultiplicativeSemigroup, Eq}
import spire.laws.MultiplicativeSemigroupLaws
import spire.std.option._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait MultiplicativeSemigroupTests[A] extends Laws {
  def laws: MultiplicativeSemigroupLaws[A]

  def MultiplicativeSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "MultiplicativeSemigroup",
      None,
      "times associative" -> forAll(laws.timesAssociative _),
      "pow1" -> forAll(laws.pow1 _),
      "pow2" -> forAll(laws.pow2 _),
      "tryProduct" -> forAll(laws.tryProduct _)
    )

}

object MultiplicativeSemigroupTests {
  def apply[A: MultiplicativeSemigroup]: MultiplicativeSemigroupTests[A] =
    new MultiplicativeSemigroupTests[A] { def laws: MultiplicativeSemigroupLaws[A] = MultiplicativeSemigroupLaws[A] }
}
