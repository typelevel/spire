package spire.laws

import spire.algebra._
import spire.algebra.partial._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object PartialGroupLaws {
  def apply[A : Eq : Arbitrary] = new PartialGroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait PartialGroupLaws[A] extends GroupLaws[A] {

  def semigroupoid(implicit A: Semigroupoid[A]) = new GroupProperties(
    name = "semigroupoid",
    parent = None,
    "associative: a |+|?? b && b |+|?? c imply (a |+| b) |+|?? c" → forAll((a: A, b: A, c: A) =>
      !((a |+|?? b) && (b |+|?? c)) || ((a |+|? b).get |+|?? c)
    ),

    "associative: (a |+|? b) |+|? c === a |+|? (b |+|? c)" → forAll((a: A, b: A, c: A) => {
      (!(a |+|?? b) || !(b |+|?? c)) ||
      ((a |+|? b).get |+|? c).get === (a |+|? (b |+|? c).get).get
    }
    )
  )

  def groupoid(implicit A: Groupoid[A]) = new GroupProperties(
    name = "groupoid",
    parent = Some(semigroupoid),
    "left identity" → forAll((a: A) =>
      (a.leftId |+|?? a) && ((a.leftId() |+|? a).get === a)
    ),

    "right identity" → forAll((a: A) =>
      (a |+|?? a.rightId) && ((a |+|? a.rightId).get === a)
    ),

    "product with inverse is always defined" → forAll((a: A) =>
      (a |+|?? a.inverse) && (a.inverse |+|?? a)
    ),

    "product with inverse is a left and right identity" → forAll((a: A, b: A) =>
      !(a |+|?? b) || (
        ((a |+|? b).get |+|? b.inverse).get === a &&
          ((a.inverse |+|? a).get |+|? b).get === b
      )
    )
  )

}

// vim: expandtab:ts=2:sw=2
