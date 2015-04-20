package spire.laws

import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Algebraic

/** 
 * Cooperative Equality = equals and hashCode agree
 */
object CooperativeEquality extends Properties("Algebraic") {

	def equalz[A](x: A, y: A) = x == y && x.hashCode == y.hashCode

	val genAlgebraicInt: Gen[Algebraic] = arbitrary[Int].map(Algebraic(_))

  	implicit val arbitraryAlgebraicInt: Arbitrary[Algebraic] = Arbitrary(genAlgebraicInt)

	property("commutative +") = forAll { (a: Algebraic, b: Algebraic) =>
    	equalz(a+b,b+a) 
  	}

	property("commutative *") = forAll { (a: Algebraic, b: Algebraic) =>
    	equalz(a*b,b*a) 
  	}

    val genAlgebraicSameInt: Gen[Algebraic] = Gen.choose(Int.MinValue, Int.MaxValue).map(Algebraic(_))

    implicit val arbitraryAlgebraicSameInt: Arbitrary[Algebraic] = Arbitrary(genAlgebraicSameInt)

	property("tranisitive ==") = forAll { (a: Algebraic, b: Algebraic, c: Algebraic) =>
      if(a == b && b == c) (a == c) else true
  	}  	
}