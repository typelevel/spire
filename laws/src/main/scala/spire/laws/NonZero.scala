package spire.laws

import spire.algebra.{AdditiveCMonoid, Eq}

import org.scalacheck.Arbitrary
import Arbitrary.arbitrary

case class NonZero[A](val a: A) extends AnyVal

object NonZero {

  implicit def arbNonZero[A:AdditiveCMonoid:Arbitrary:Eq]: Arbitrary[NonZero[A]] =
  Arbitrary { arbitrary[A].filter(!AdditiveCMonoid[A].isZero(_)).map(NonZero(_)) }

}
