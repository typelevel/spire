package spire.laws
package discipline

import spire.algebra.{AbGroup, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait AbGroupTests[A] extends CMonoidTests[A] with GroupTests[A] {
  def laws: AbGroupLaws[A]

  def AbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "AbGroup"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(CMonoid, Group)
      val props: Seq[(String, Prop)] = Nil
    }

}

object AbGroupTests {
  def apply[A: AbGroup]: AbGroupTests[A] =
    new AbGroupTests[A] { def laws: AbGroupLaws[A] = AbGroupLaws[A] }
}
