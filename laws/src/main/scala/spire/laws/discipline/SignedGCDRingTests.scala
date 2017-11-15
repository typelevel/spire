package spire.laws
package discipline

import spire.algebra.{Eq, GCDRing, Signed}
import spire.std.boolean._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SignedGCDRingTests[A] extends SignedAdditiveAbGroupTests[A] with GCDRingTests[A] {

  def laws: SignedGCDRingLaws[A]

  def signedGCDRing(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "signedGCDRing",
      Some(signedAdditiveAbGroup),
      "gcd sign" -> forAll(laws.gcdSign _),
      "gcd by zero" -> forAll(laws.gcdByZero _)
    )
}

object SignedGCDRingTests {
  def apply[A: GCDRing: Signed]: SignedGCDRingTests[A] =
    new SignedGCDRingTests[A] { def laws: SignedGCDRingLaws[A] = SignedGCDRingLaws[A] }
}
