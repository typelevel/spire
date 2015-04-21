package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.{Complex, Rational, SafeLong}
import spire.math.Complex._

/** 
 * Cooperative Equality = equals and hashCode agree
 */
object ComplexCoopEqProp extends Properties("Complex") {

	// val genComplex: Gen[Complex] = for {
	// 	f <- arbitrary[Float]
	// 	d <- arbitrary[Double]
	// } yield Complex(f, d)


}