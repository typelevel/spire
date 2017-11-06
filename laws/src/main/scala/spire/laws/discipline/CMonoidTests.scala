package spire.laws
package discipline

import spire.algebra.{CMonoid, Eq}

import org.scalacheck.{Arbitrary, Prop}

trait CMonoidTests[A] extends CSemigroupTests[A] with MonoidTests[A] {
  def laws: CMonoidLaws[A]

  def CMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "CMonoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(CSemigroup, Monoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CMonoidTests {
  def apply[A: CMonoid]: CMonoidTests[A] =
    new CMonoidTests[A] { def laws: CMonoidLaws[A] = CMonoidLaws[A] }
}
