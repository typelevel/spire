package spire.laws
package discipline

import spire.algebra.{MultiplicativeAbGroup, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait MultiplicativeAbGroupTests[A] extends MultiplicativeCMonoidTests[A] with MultiplicativeGroupTests[A] {
  def laws: MultiplicativeAbGroupLaws[A]

  def MultiplicativeAbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "MultiplicativeAbGroup"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(MultiplicativeCMonoid, MultiplicativeGroup)
      val props: Seq[(String, Prop)] = Nil
    }

}

object MultiplicativeAbGroupTests {
  def apply[A: MultiplicativeAbGroup]: MultiplicativeAbGroupTests[A] =
    new MultiplicativeAbGroupTests[A] { def laws: MultiplicativeAbGroupLaws[A] = MultiplicativeAbGroupLaws[A] }
}
