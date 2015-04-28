package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Test.Parameters

import spire.math.{Number, SafeLong, Rational}

object NumberCoopEqProp extends Properties("Number") {
	

	val genInt: Gen[Number]        = arbitrary[Int].map(Number(_))
	val genLong: Gen[Number]       = arbitrary[Long].map(Number(_))
	val genBigInt: Gen[Number]     = arbitrary[BigInt].map(Number(_))
	val genSafeLong: Gen[Number]   = Utility.genSafeLong.map(Number(_))
	val genRational: Gen[Number]   = Utility.genRational.map(Number(_))

	val intProps: List[Prop]  = Utility.props[Number](genInt)
	val longProps: List[Prop] = Utility.props[Number](genLong)
	val bigIntProps: List[Prop] = Utility.props[Number](genBigInt)
	val safeLongProps: List[Prop] = Utility.props[Number](genSafeLong)
	val rationalProps: List[Prop] = Utility.props[Number](genRational)


    Utility.printPreTestRun(Utility.NUMBER, Utility.INT)
  	intProps.foreach(_.check)

    Utility.printPreTestRun(Utility.NUMBER, Utility.LONG)
  	longProps.foreach(_.check)  	

    Utility.printPreTestRun(Utility.NUMBER, Utility.BIG_INT)
  	bigIntProps.foreach(_.check)  	

    Utility.printPreTestRun(Utility.NUMBER, Utility.SAFE_LONG)
  	safeLongProps.foreach(_.check)  	 	

    Utility.printPreTestRun(Utility.NUMBER, Utility.RATIONAL)
  	rationalProps.foreach(_.check)  	 	  	

}