package spire.laws
package discipline

import spire.algebra.{AbGroup, Eq}

import org.scalacheck.Arbitrary

trait AbGroupTests[A] extends CMonoidTests[A] with GroupTests[A] {
  def laws: AbGroupLaws[A]

  def abGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new CommutativeRuleSet(
      "abGroup",
      group,
      Some(cMonoid)
    )
}

object AbGroupTests {
  def apply[A: AbGroup]: AbGroupTests[A] =
    new AbGroupTests[A] { def laws: AbGroupLaws[A] = AbGroupLaws[A] }
}
