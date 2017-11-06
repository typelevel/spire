package spire.laws.discipline

import spire.algebra.{AdditiveSemigroup, Eq}
import spire.laws.AdditiveSemigroupLaws
import spire.std.option._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait AdditiveSemigroupTests[A] extends Laws {
  def laws: AdditiveSemigroupLaws[A]

  def additiveSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "additiveSemigroup",
      None,
      "plus associative" -> forAll(laws.plusAssociative _),
      "sum1" -> forAll(laws.sum1 _),
      "sum2" -> forAll(laws.sum2 _),
      "trySum" -> forAll(laws.trySum _)
    )

}

object AdditiveSemigroupTests {
  def apply[A: AdditiveSemigroup]: AdditiveSemigroupTests[A] =
    new AdditiveSemigroupTests[A] { def laws: AdditiveSemigroupLaws[A] = AdditiveSemigroupLaws[A] }
}
