package spire.laws
package discipline

import spire.algebra.{AdditiveCMonoid, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait AdditiveCMonoidTests[A] extends AdditiveCSemigroupTests[A] with AdditiveMonoidTests[A] {
  def laws: AdditiveCMonoidLaws[A]

  def additiveCMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "additiveCMonoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(additiveCSemigroup, additiveMonoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object AdditiveCMonoidTests {
  def apply[A: AdditiveCMonoid]: AdditiveCMonoidTests[A] =
    new AdditiveCMonoidTests[A] { def laws: AdditiveCMonoidLaws[A] = AdditiveCMonoidLaws[A] }
}
