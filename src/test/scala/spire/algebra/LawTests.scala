package spire.algebra

import spire.implicits._
import spire.math._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class LawTests extends LawChecker {

  implicit def RealArbitrary: Arbitrary[Real] = Arbitrary(arbitrary[Int].map(Real(_)))

  checkAll("BigInt", Laws[BigInt].euclideanRing)
  checkAll("Real", Laws[Real].field)

}

// vim: expandtab:ts=2:sw=2
