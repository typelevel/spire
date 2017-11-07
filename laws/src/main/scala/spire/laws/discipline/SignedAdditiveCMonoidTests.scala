package spire.laws
package discipline

import spire.algebra.{AdditiveCMonoid, Eq, Signed}
import spire.std.boolean._
import spire.std.int._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SignedAdditiveCMonoidTests[A] extends SignedTests[A] with AdditiveCMonoidTests[A] {

  def laws: SignedAdditiveCMonoidLaws[A]

  def signedAdditiveCMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "signedAdditiveCMonoid",
      Some(signed),
      "ordered group" -> forAll(laws.orderedGroup _),
      "triangle inequality" -> forAll(laws.triangleInequality _)
    )
}

object SignedAdditiveCMonoidTests {
  def apply[A: AdditiveCMonoid: Signed]: SignedAdditiveCMonoidTests[A] =
    new SignedAdditiveCMonoidTests[A] { def laws: SignedAdditiveCMonoidLaws[A] = SignedAdditiveCMonoidLaws[A] }
}
