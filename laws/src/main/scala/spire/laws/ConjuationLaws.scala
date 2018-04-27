package spire.laws

import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import spire.algebra.{Conjugation, Eq, MultiplicativeMonoid, Ring}
import spire.syntax.eq._

import InvalidTestException._

object ConjugationLaws {
  def apply[A: Eq: Arbitrary] = new ConjugationLaws[A] {
    def Equ: Eq[A] = Eq[A]
    def Arb: Arbitrary[A] = implicitly[Arbitrary[A]]
  }
}

trait ConjugationLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def conjugation(implicit A: Conjugation[A]) = new DefaultRuleSet(
    name = "conjugation",
    parent = None,
    "involution" -> forAllSafe( (x: A) => A.conjugate(A.conjugate(x)) === x )
  )

  def conjugationMultiplicativeMonoid(implicit A: Conjugation[A], mm: MultiplicativeMonoid[A]) =  new DefaultRuleSet(
    name = "conjugationMultiplicativeMonoid",
    parent = Some(conjugation),
    "preserves one" -> (A.conjugate(mm.one) === mm.one),
    "antiautomorphism" -> forAllSafe( (x: A, y: A) => A.conjugate(mm.times(x, y)) === mm.times(A.conjugate(y), A.conjugate(x)))
  )

  def conjugationRing(implicit A: Conjugation[A], ringA: Ring[A]) = new DefaultRuleSet(
    name = "conjugationRing",
    parent = Some(conjugationMultiplicativeMonoid),
    "compatible with addition" -> forAllSafe( (x: A, y: A) => A.conjugate(ringA.plus(x, y)) === ringA.plus(A.conjugate(x), A.conjugate(y))),
  )
}
