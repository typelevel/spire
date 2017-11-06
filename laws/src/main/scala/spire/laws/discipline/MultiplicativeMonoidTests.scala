package spire.laws.discipline

import spire.algebra.{MultiplicativeMonoid, Eq}
import spire.laws.MultiplicativeMonoidLaws
import spire.std.boolean._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MultiplicativeMonoidTests[A] extends MultiplicativeSemigroupTests[A] {

  def laws: MultiplicativeMonoidLaws[A]

  def MultiplicativeMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "MultiplicativeMonoid",
      Some(MultiplicativeSemigroup),
      "left one" -> forAll(laws.leftOne _),
      "right one" -> forAll(laws.rightOne _),
      "pow0" -> forAll(laws.pow0 _),
      "product" -> laws.product,
      "isOne" -> forAll((a: A) => laws.isOne(a, eqA))
    )
}

object MultiplicativeMonoidTests {
  def apply[A: MultiplicativeMonoid]: MultiplicativeMonoidTests[A] =
    new MultiplicativeMonoidTests[A] { def laws: MultiplicativeMonoidLaws[A] = MultiplicativeMonoidLaws[A] }
}
