package spire.algebra

import spire.math._

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

object SpireArbitrary {

  implicit def RationalArbitrary: Arbitrary[Rational] = Arbitrary(Gen.oneOf(
    arbitrary[Int] map (Rational(_)),
    arbitrary[Double] map (Rational(_))
  ))

  implicit def RealArbitrary: Arbitrary[Real] = Arbitrary(arbitrary[Int] map (Real(_)))

  implicit def VectorArbitrary[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(arbitrary[List[A]] map (Vector(_: _*)))

}

// vim: expandtab:ts=2:sw=2
