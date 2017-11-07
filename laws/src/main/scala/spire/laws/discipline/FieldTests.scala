package spire.laws
package discipline

import spire.algebra.{Field, Eq}

import org.scalacheck.Arbitrary

trait FieldTests[A] extends DivisionRingTests[A] with EuclideanRingTests[A] {
  def laws: FieldLaws[A]

  def field(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      def name = "divisionRing"
      def parents = Seq(divisionRing, euclideanRing)
      def bases = Nil
      def props = Nil
    }
}

object FieldTests {
  def apply[A: Field]: FieldTests[A] =
    new FieldTests[A] { def laws: FieldLaws[A] = FieldLaws[A] }
}
