package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.SafeLong

object SafeLongCoopEqProp extends Properties("SafeLong") {
	// long, bi, string

	val genLong: Gen[SafeLong]   = arbitrary[Long].map(SafeLong(_))
	val genBigInt: Gen[SafeLong] = arbitrary[BigInt].map(SafeLong(_))
	val genString: Gen[SafeLong] = arbitrary[BigInt].map(bi => SafeLong(bi.toString))

	val longProps: List[Prop]   = Utility.props[SafeLong](genLong)
	val bigIntProps: List[Prop] = Utility.props[SafeLong](genBigInt)
	val stringProps: List[Prop] = Utility.props[SafeLong](genString)
  	
  	Utility.printPreTestRun(Utility.SAFE_LONG, Utility.LONG)
  	longProps.foreach(_.check)	

  	Utility.printPreTestRun(Utility.SAFE_LONG, Utility.BIG_INT)
  	bigIntProps.foreach(_.check)	

	Utility.printPreTestRun(Utility.SAFE_LONG, Utility.STRING)
  	stringProps.foreach(_.check)	  	
}