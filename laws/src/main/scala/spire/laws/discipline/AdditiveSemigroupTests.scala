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
      "sumN1" -> forAll(laws.sumN1 _),
      "sumN2" -> forAll(laws.sumN2 _),
      "sumN3" -> forAll(laws.sumN3 _),
      "trySum" -> forAll(laws.trySum _)
    )

}

object AdditiveSemigroupTests {
  def apply[A: AdditiveSemigroup]: AdditiveSemigroupTests[A] =
    new AdditiveSemigroupTests[A] { def laws: AdditiveSemigroupLaws[A] = AdditiveSemigroupLaws[A] }
}
