package spire.laws
package discipline

import spire.algebra.{MultiplicativeCMonoid, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait MultiplicativeCMonoidTests[A] extends MultiplicativeCSemigroupTests[A] with MultiplicativeMonoidTests[A] {
  def laws: MultiplicativeCMonoidLaws[A]

  def MultiplicativeCMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "MultiplicativeCMonoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(MultiplicativeCSemigroup, MultiplicativeMonoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object MultiplicativeCMonoidTests {
  def apply[A: MultiplicativeCMonoid]: MultiplicativeCMonoidTests[A] =
    new MultiplicativeCMonoidTests[A] { def laws: MultiplicativeCMonoidLaws[A] = MultiplicativeCMonoidLaws[A] }
}
