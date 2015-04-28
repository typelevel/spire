package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Test.Parameters

import spire.math.{Quaternion, Real, SafeLong, Rational}
import spire.implicits._

object QuaternionCoopEqProp extends Properties("Quaternion") { 

	val genFloat: Gen[Quaternion[Float]]       = arbitrary[Float].map(Quaternion(_))
	val genDouble: Gen[Quaternion[Double]]     = arbitrary[Double].map(Quaternion(_))
	//TODO: val genReal: Gen[Quaternion[Real]]         = arbitrary[Real].map(Quaternion(_))
	val genRational: Gen[Quaternion[Rational]] = Utility.genRational.map(Quaternion(_))
	val genSafeLong: Gen[Quaternion[SafeLong]] = Utility.genSafeLong.map(Quaternion(_))

	val floatProps: List[Prop]    = Utility.props[Quaternion[Float]](genFloat)
	val doubleProps: List[Prop]   = Utility.props[Quaternion[Double]](genDouble)
	// val rationalProps: List[Prop] = Utility.props[Quaternion[Rational]](genRational)
	// val safeLongProps: List[Prop] = Utility.props[Quaternion[SafeLong]](genSafeLong)

  	Utility.printPreTestRun(Utility.QUATERNION, Utility.FLOAT)
 	floatProps.foreach(_.check)

  	Utility.printPreTestRun(Utility.QUATERNION, Utility.FLOAT)
 	floatProps.foreach(_.check) 	
}
