package spire.laws
package discipline

import spire.algebra.{CMonoid, Eq}

import org.scalacheck.Arbitrary

trait CMonoidTests[A] extends CSemigroupTests[A] with MonoidTests[A] {
  def laws: CMonoidLaws[A]

  def cMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "cMonoid",
      monoid,
      Some(cSemigroup)
    )
}

object CMonoidTests {
  def apply[A: CMonoid]: CMonoidTests[A] =
    new CMonoidTests[A] { def laws: CMonoidLaws[A] = CMonoidLaws[A] }
}
