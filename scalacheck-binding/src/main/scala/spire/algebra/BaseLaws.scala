package spire.algebra

import spire.implicits._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object BaseLaws {
  def apply[A : Eq : Arbitrary] = new BaseLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait BaseLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]


  def signed(implicit A: Signed[A]) = new SimpleProperties(
    name = "signed",
    "abs non-negative" → forAll((x: A) =>
      x.abs.sign != Negative
    )
  )

  def metricSpace[R](implicit MSA: MetricSpace[A, R], SR: Signed[R], OR: Order[R], ASR: AdditiveSemigroup[R]) = new SimpleProperties(
    name = "metricSpace",
    "non-negative" → forAll((a1: A, a2: A) =>
      MSA.distance(a1, a2).sign != Negative
    ),
    "identity" → forAll((a: A) =>
      MSA.distance(a, a).sign == Zero
    ),
    "equality" → forAll((a1: A, a2: A) =>
      // generating equal values is hard, and Scalacheck will give up if it can't
      // hence, not using `==>` here
      a1 =!= a2 || MSA.distance(a1, a2).sign == Zero
    ),
    "symmetry" → forAll((a1: A, a2: A) =>
      MSA.distance(a1, a2) === MSA.distance(a2, a1)
    ),
    "triangleInequality" → forAll((a1: A, a2: A, a3: A) =>
      (MSA.distance(a1, a2) + MSA.distance(a2, a3)) >= MSA.distance(a1, a3)
    )
  )

}

// vim: expandtab:ts=2:sw=2
