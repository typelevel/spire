package spire
package laws

import spire.algebra._
import spire.algebra.partial._
import spire.implicits._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import InvalidTestException._

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
    "associative: a |+|?? b && b |+|?? c imply (a |+| b) |+|?? c" -> forAllSafe((a: A, b: A, c: A) =>
      !((a |+|?? b) && (b |+|?? c)) || ((a |+|? b).get |+|?? c)
    ),

    "associative: (a |+|? b) |+|? c === a |+|? (b |+|? c)" -> forAllSafe((a: A, b: A, c: A) => {
      (!(a |+|?? b) || !(b |+|?? c)) ||
      ((a |+|? b).get |+|? c).get === (a |+|? (b |+|? c).get).get
    }
    )
  )

  def groupoid(implicit A: Groupoid[A]) = new GroupProperties(
    name = "groupoid",
    parent = Some(semigroupoid),
    "left identity" -> forAllSafe((a: A) =>
      (a.leftId |+|?? a) && ((a.leftId() |+|? a).get === a)
    ),

    "right identity" -> forAllSafe((a: A) =>
      (a |+|?? a.rightId) && ((a |+|? a.rightId).get === a)
    ),

    "product with inverse is always defined" -> forAllSafe((a: A) =>
      (a |+|?? a.inverse) && (a.inverse |+|?? a)
    ),

    "product with inverse is a left and right identity" -> forAllSafe((a: A, b: A) =>
      !(a |+|?? b) || (
        ((a |+|? b).get |+|? b.inverse).get === a &&
          ((a.inverse |+|? a).get |+|? b).get === b
      )
    )
  )

}

// vim: expandtab:ts=2:sw=2
