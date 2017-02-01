package spire.laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object HomomorphismLaws {
  def apply[A : Eq : Arbitrary]: HomomorphismLaws[A] = new HomomorphismLaws[A] {
    def Arb = implicitly[Arbitrary[A]]
    def Equ = implicitly[Eq[A]]
  }
}

trait HomomorphismLaws[A] extends Laws {

  implicit def Arb: Arbitrary[A]

  implicit def Equ: Eq[A]

  def cRing[B](implicit A: CRing[A], B: CRing[B], EqB: Eq[B], H: Homomorphism[A, B, CRing]) = new DefaultRuleSet(
    name = "Homomorphism[_, _, CRing]",
    parent = None,
    "preserves identity" → {
      H(A.one) === B.one
    },
    "preserves addition" → forAll { (x: A, y: A) =>
      H(A.plus(x, y)) === B.plus(H(x), H(y))
    },
    "preserves multiplication" → forAll { (x: A, y: A) =>
      H(A.times(x, y)) === B.times(H(x), H(y))
    }
  )

}
