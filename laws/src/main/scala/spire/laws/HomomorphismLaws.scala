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

  def semigroup[B, F[X] <: Semigroup[X]](implicit A: Semigroup[A], B: Semigroup[B], EqB: Eq[B], H: Homomorphism[A, B, F]) = new DefaultRuleSet(
    name = "Homomorphism[_, _, Semigroup]",
    parent = None,
    "preserves combine" → forAll { (x: A, y: A) =>
      H(A.combine(x, y)) === B.combine(H(x), H(y))
    }
  )

  def monoid[B, F[X] <: Semigroup[X]](implicit A: Monoid[A], B: Monoid[B], EqB: Eq[B], H: Homomorphism[A, B, F]) = new DefaultRuleSet(
    name = "Homomorphism[_, _, Semigroup]",
    parent = Some(semigroup[B, F]),
    "preserves empty" → {
      H(A.empty) === B.empty
    }
  )

  def ring[B, F[X] <: Ring[X]](implicit A: Ring[A], B: Ring[B], EqB: Eq[B], H: Homomorphism[A, B, F]) = new DefaultRuleSet(
    name = "Homomorphism[_, _, Ring]",
    parent = None,
    "preserves one" → {
      H(A.one) === B.one
    },
    "preserves addition" → forAll { (x: A, y: A) =>
      H(A.plus(x, y)) === B.plus(H(x), H(y))
    },
    "preserves multiplication" → forAll { (x: A, y: A) =>
      H(A.times(x, y)) === B.times(H(x), H(y))
    }
  )

  def field[B, F[X] <: Field[X]](implicit A: Field[A], B: Field[B], EqB: Eq[B], H: Homomorphism[A, B, F]) = new DefaultRuleSet(
    name = "Homomorphism[_, _, Field]",
    parent = Some(ring[B, F]),
    "preserves zero" → {
      H(A.zero) === B.zero
    }
  )

}
