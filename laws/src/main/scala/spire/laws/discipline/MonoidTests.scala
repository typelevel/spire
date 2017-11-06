package spire.laws.discipline

import spire.algebra.{Monoid, Eq}
import spire.laws.MonoidLaws
import spire.std.boolean._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonoidTests[A] extends SemigroupTests[A] {

  def laws: MonoidLaws[A]

  def Monoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "Monoid",
      Some(Semigroup),
      "left identity" -> forAll(laws.leftEmpty _),
      "right identity" -> forAll(laws.rightEmpty _),
      "combineN0" -> forAll(laws.combineN0 _),
      "combineAll" -> laws.combineAll,
      "isEmpty" -> forAll((a: A) => laws.isEmpty(a, eqA))
    )
}

object MonoidTests {
  def apply[A: Monoid]: MonoidTests[A] =
    new MonoidTests[A] { def laws: MonoidLaws[A] = MonoidLaws[A] }
}
