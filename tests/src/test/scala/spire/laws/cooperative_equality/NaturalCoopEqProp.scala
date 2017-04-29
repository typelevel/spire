package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Natural
import spire.implicits._
import spire.math.NaturalIsRig

object NaturalCoopEqProp extends Properties("NaturalCoopEqProp") {

	val genInt: Gen[Natural]    = Gen.choose(1, Int.MaxValue).map(Natural(_))
	val genBigInt: Gen[Natural] = Gen.choose(1, Long.MaxValue).map{ x: Long => Natural(BigInt(x)) }
	val genLong: Gen[Natural]   = Gen.choose(1, Long.MaxValue).map(Natural(_))

  	println(s"Checking Naturals's Cooperative Equality for Int")
  	Utility.coopEqProp(genInt).check

	println(s"Checking Naturals's Cooperative Equality for BigInt")
	Utility.coopEqProp(genBigInt).check

  	println(s"Checking Naturals's Cooperative Equality for Long")
  	Utility.coopEqProp(genLong).check  
}
