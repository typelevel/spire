package spire.laws

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import spire.math.Algebraic

object CooperativeEquality extends Properties("Algebraic") {

    val genAlgebraicInt: Gen[Algebraic] = for {
    	x <- arbitrary[Int]
  	} yield Algebraic(x)


  	implicit val arbitraryAlgebraicInt: Arbitrary[Algebraic] = Arbitrary(genAlgebraicInt)

	property("commutative +") = forAll { (a: Algebraic, b: Algebraic) =>
    	(a+b) == (b+a)
  	}

	property("commutative *") = forAll { (a: Algebraic, b: Algebraic) =>
    	(a*b) == (b*a)
  	}  	
}