package spire.laws.discipline

import spire.algebra.{AdditiveMonoid, Eq}
import spire.laws.AdditiveMonoidLaws
import spire.std.boolean._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait AdditiveMonoidTests[A] extends AdditiveSemigroupTests[A] {

  def laws: AdditiveMonoidLaws[A]

  def additiveMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "additiveMonoid",
      Some(additiveSemigroup),
      "left zero" -> forAll(laws.leftZero _),
      "right zero" -> forAll(laws.rightZero _),
      "sum0" -> forAll(laws.sum0 _),
      "sumAll" -> laws.sumAll,
      "isZero" -> forAll((a: A) => laws.isZero(a, eqA))
    )
}

object AdditiveMonoidTests {
  def apply[A: AdditiveMonoid]: AdditiveMonoidTests[A] =
    new AdditiveMonoidTests[A] { def laws: AdditiveMonoidLaws[A] = AdditiveMonoidLaws[A] }
}
