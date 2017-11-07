package spire.laws
package discipline

import spire.algebra.{DivisionRing, Eq}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait DivisionRingTests[A] extends RingTests[A] {
  def laws: DivisionRingLaws[A]

  def divisionRing(implicit arbA: Arbitrary[A], eqA: Eq[A]): RingRuleSet = {
    implicit def S: DivisionRing[A] = laws.S
    new RingRuleSet(
      "divisionRing",
      additiveAbGroup,
      multiplicativeMonoid,
      Seq(ring),
      "left reciprocal" -> forAll(laws.leftReciprocal _),
      "right reciprocal" -> forAll(laws.rightReciprocal _),
      "consistent div" -> forAll(laws.consistentDiv _)
    )
  }
}

object DivisionRingTests {
  def apply[A: DivisionRing]: DivisionRingTests[A] =
    new DivisionRingTests[A] { def laws: DivisionRingLaws[A] = DivisionRingLaws[A] }
}
