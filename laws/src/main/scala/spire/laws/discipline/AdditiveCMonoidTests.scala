package spire.laws
package discipline

import spire.algebra.{AdditiveCMonoid, Eq}

import org.scalacheck.Arbitrary

trait AdditiveCMonoidTests[A] extends AdditiveCSemigroupTests[A] with AdditiveMonoidTests[A] {
  def laws: AdditiveCMonoidLaws[A]

  def additiveCMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "additiveCMonoid",
      additiveMonoid,
      Some(additiveCSemigroup)
    )
}

object AdditiveCMonoidTests {
  def apply[A: AdditiveCMonoid]: AdditiveCMonoidTests[A] =
    new AdditiveCMonoidTests[A] { def laws: AdditiveCMonoidLaws[A] = AdditiveCMonoidLaws[A] }
}
