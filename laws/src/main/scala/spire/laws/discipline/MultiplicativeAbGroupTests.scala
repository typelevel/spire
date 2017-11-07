package spire.laws
package discipline

import spire.algebra.{MultiplicativeAbGroup, Eq}

import org.scalacheck.Arbitrary

trait MultiplicativeAbGroupTests[A] extends MultiplicativeCMonoidTests[A] with MultiplicativeGroupTests[A] {
  def laws: MultiplicativeAbGroupLaws[A]

  def multiplicativeAbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): CommutativeRuleSet =
    new CommutativeRuleSet(
      "multiplicativeAbGroup",
      multiplicativeGroup,
      Some(multiplicativeCMonoid)
    )
}

object MultiplicativeAbGroupTests {
  def apply[A: MultiplicativeAbGroup]: MultiplicativeAbGroupTests[A] =
    new MultiplicativeAbGroupTests[A] { def laws: MultiplicativeAbGroupLaws[A] = MultiplicativeAbGroupLaws[A] }
}
