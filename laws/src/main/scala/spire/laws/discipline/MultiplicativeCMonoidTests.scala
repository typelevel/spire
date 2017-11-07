package spire.laws
package discipline

import spire.algebra.{MultiplicativeCMonoid, Eq}

import org.scalacheck.Arbitrary

trait MultiplicativeCMonoidTests[A] extends MultiplicativeCSemigroupTests[A] with MultiplicativeMonoidTests[A] {
  def laws: MultiplicativeCMonoidLaws[A]

  def multiplicativeCMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "multiplicativeCMonoid",
      multiplicativeMonoid,
      Some(multiplicativeCSemigroup)
    )
}

object MultiplicativeCMonoidTests {
  def apply[A: MultiplicativeCMonoid]: MultiplicativeCMonoidTests[A] =
    new MultiplicativeCMonoidTests[A] { def laws: MultiplicativeCMonoidLaws[A] = MultiplicativeCMonoidLaws[A] }
}
