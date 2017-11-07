package spire.laws
package discipline

import spire.algebra.{AdditiveAbGroup, Eq, Signed}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SignedAdditiveAbGroupTests[A] extends SignedAdditiveCMonoidTests[A] with AdditiveAbGroupTests[A] {

  def laws: SignedAdditiveAbGroupLaws[A]

  def signedAdditiveAbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "signedAdditiveAbGroup",
      Some(signedAdditiveCMonoid),
      "abs negate" -> forAll(laws.absNegate _)
    )
}

object SignedAdditiveAbGroupTests {
  def apply[A: AdditiveAbGroup: Signed]: SignedAdditiveAbGroupTests[A] =
    new SignedAdditiveAbGroupTests[A] { def laws: SignedAdditiveAbGroupLaws[A] = SignedAdditiveAbGroupLaws[A] }
}
