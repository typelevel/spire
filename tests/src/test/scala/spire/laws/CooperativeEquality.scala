package spire.laws

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Algebraic

/** 
 * Cooperative Equality = equals and hashCode agree
 */
object CooperativeEquality extends Properties("Algebraic") {

	def coopEquals[A](x: A, y: A) = x == y && x.hashCode == y.hashCode

	val genAlgebraicInt: Gen[Algebraic] = arbitrary[Int].map(Algebraic(_))

	property("commutative +") = forAll(genAlgebraicInt, genAlgebraicInt) { (a: Algebraic, b: Algebraic) =>
  	coopEquals(a+b,b+a) 
	}

	property("commutative *") = forAll(genAlgebraicInt, genAlgebraicInt) { (a: Algebraic, b: Algebraic) =>
  	coopEquals(a*b,b*a) 
	}

  //TODO: improve since only a handful of the 100 runs could 'truly' evaluate to true
	property("tranisitive ==") = forAll(genAlgebraicInt, genAlgebraicInt, genAlgebraicInt) { (a: Algebraic, b: Algebraic, c: Algebraic) =>
    if( coopEquals(a, b) && coopEquals(b, c)) coopEquals(a, c) else true
	}

}