package spire.laws
package discipline

import spire.algebra.{AdditiveAbGroup, Eq}

import org.scalacheck.Arbitrary

trait AdditiveAbGroupTests[A] extends AdditiveCMonoidTests[A] with AdditiveGroupTests[A] {
  def laws: AdditiveAbGroupLaws[A]

  def additiveAbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "additiveAbGroup",
      additiveGroup,
      Some(additiveCMonoid)
    )
}

object AdditiveAbGroupTests {
  def apply[A: AdditiveAbGroup]: AdditiveAbGroupTests[A] =
    new AdditiveAbGroupTests[A] { def laws: AdditiveAbGroupLaws[A] = AdditiveAbGroupLaws[A] }
}
