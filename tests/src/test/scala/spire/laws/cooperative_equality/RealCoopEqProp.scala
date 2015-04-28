package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Real

object RealCoopEqProp extends Properties("Real") {
	
	val genInt: Gen[Real]      = arbitrary[Int].map(Real(_))
	val genLong: Gen[Real]     = arbitrary[Long].map(Real(_))
	val genBigInt: Gen[Real]   = arbitrary[BigInt].map(Real(_))
	val genSafeLong: Gen[Real] = Utility.genSafeLong.map(Real(_))
	val genRational: Gen[Real] = Utility.genRational.map(Real(_))
	val genDouble: Gen[Real]   = arbitrary[Double].map(Real(_))

	val intProps: List[Prop]      = Utility.props[Real](genInt)
	val longProps: List[Prop]     = Utility.props[Real](genLong)
	val bigIntProps: List[Prop]   = Utility.props[Real](genBigInt)
	val safeLongProps: List[Prop] = Utility.props[Real](genSafeLong)
	val rationalProps: List[Prop] = Utility.props[Real](genRational)
	val doubleProps: List[Prop]   = Utility.props[Real](genDouble)
  	
  	Utility.printPreTestRun(Utility.REAL, Utility.INT)
  	intProps.foreach(_.check)	

  	Utility.printPreTestRun(Utility.REAL, Utility.LONG)
  	longProps.foreach(_.check)	  	

  	Utility.printPreTestRun(Utility.REAL, Utility.BIG_INT)
  	bigIntProps.foreach(_.check)

  	Utility.printPreTestRun(Utility.REAL, Utility.SAFE_LONG)
  	safeLongProps.foreach(_.check)	

  	Utility.printPreTestRun(Utility.REAL, Utility.RATIONAL)
  	rationalProps.foreach(_.check)	  	

  	Utility.printPreTestRun(Utility.REAL, Utility.DOUBLE)
  	doubleProps.foreach(_.check)	  	

}
