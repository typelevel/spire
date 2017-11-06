package spire.laws
package discipline

import spire.algebra.{AdditiveAbGroup, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait AdditiveAbGroupTests[A] extends AdditiveCMonoidTests[A] with AdditiveGroupTests[A] {
  def laws: AdditiveAbGroupLaws[A]

  def additiveAbGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "additiveAbGroup"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(additiveCMonoid, additiveGroup)
      val props: Seq[(String, Prop)] = Nil
    }

}

object AdditiveAbGroupTests {
  def apply[A: AdditiveAbGroup]: AdditiveAbGroupTests[A] =
    new AdditiveAbGroupTests[A] { def laws: AdditiveAbGroupLaws[A] = AdditiveAbGroupLaws[A] }
}
