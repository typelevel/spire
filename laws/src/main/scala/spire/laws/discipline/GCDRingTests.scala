package spire.laws
package discipline

import spire.algebra.{GCDRing, Eq}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait GCDRingTests[A] extends CRingTests[A] {
  def laws: GCDRingLaws[A]

  def gcdRing(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "gcdRing",
      Some(cRing),
      "gcd lcm product" -> forAll((x: A, y: A) => laws.gcdLcmProduct(x, y, eqA)),
      "gcd commutative" -> forAll((x: A, y: A) => laws.gcdCommutative(x, y, eqA)),
      "lcm commutative" -> forAll((x: A, y: A) => laws.lcmCommutative(x, y, eqA)),
      "gcd zero zero" -> laws.gcdZeroZero(eqA),
      "lcm zero zero" -> laws.lcmZeroZero(eqA),
      "lcm zero" -> forAll((a: A) => laws.lcmZero(a, eqA))
    )
}

object GCDRingTests {
  def apply[A: GCDRing]: GCDRingTests[A] =
    new GCDRingTests[A] { def laws: GCDRingLaws[A] = GCDRingLaws[A] }
}
