package spire.laws
package discipline

import spire.algebra.{EuclideanRing, Eq}
import spire.std.boolean._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait EuclideanRingTests[A] extends GCDRingTests[A] {
  def laws: EuclideanRingLaws[A]

  def euclideanRing(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet = {
    implicit def S: EuclideanRing[A] = laws.S
    new DefaultRuleSet(
      "euclideanRing",
      Some(gcdRing),
      "euclidean division rule" -> forAll(laws.euclideanDivisionRule _),
      "equot" -> forAll(laws.equot _),
      "emod" -> forAll(laws.emod _),
      "euclidean function" -> forAll( (x: A, y: NonZero[A]) => laws.euclideanFunction(x, y, eqA) ),
      "submultiplicative euclidean function" -> forAll(laws.submultiplicativeEuclideanFunction _)
    )
  }
}

object EuclideanRingTests {
  def apply[A: EuclideanRing]: EuclideanRingTests[A] =
    new EuclideanRingTests[A] { def laws: EuclideanRingLaws[A] = EuclideanRingLaws[A] }
}
