package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object OldOrderLaws {
  def apply[A : Eq : Arbitrary] = new OldOrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait OldOrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def partialOrder(implicit A: PartialOrder[A]) = new OrderProperties(
    name = "partialOrder",
    parent = None,
    "reflexitivity" → forAll((x: A) =>
      x <= x
    ),
    "antisymmetry" → forAll((x: A, y: A) =>
      (x <= y && y <= x) imp (x === y)
    ),
    "transitivity" → forAll((x: A, y: A, z: A) =>
      (x <= y && y <= z) imp (x <= z)
    ),
    "gteqv" → forAll((x: A, y: A) =>
      (x <= y) === (y >= x)
    ),
    "lt" → forAll((x: A, y: A) =>
      (x < y) === (x <= y && x =!= y)
    ),
    "gt" → forAll((x: A, y: A) =>
      (x < y) === (y > x)
    )
  )

  def order(implicit A: Order[A]) = new OrderProperties(
    name = "order",
    parent = Some(partialOrder),
    "totality" → forAll((x: A, y: A) =>
      x <= y || y <= x
    )
  )

  class OrderProperties(
    name: String,
    parent: Option[OrderProperties],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

}

// vim: expandtab:ts=2:sw=2
