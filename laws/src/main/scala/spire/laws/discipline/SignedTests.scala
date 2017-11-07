package spire.laws
package discipline

import spire.algebra.{Eq, Signed}
import spire.std.boolean._
import spire.std.int._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SignedTests[A] extends OrderTests[A] {

  def laws: SignedLaws[A]

  def signed(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "signed",
      Some(order),
      "abs nonnegative" -> forAll(laws.absNonnegative _),
      "signum range" -> forAll(laws.signumRange _),
      "signum is sign.toInt" -> forAll(laws.signumIsSignToInt _)
    )
}

object SignedTests {
  def apply[A: Signed]: SignedTests[A] =
    new SignedTests[A] { def laws: SignedLaws[A] = SignedLaws[A] }
}
