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

}

// vim: expandtab:ts=2:sw=2
