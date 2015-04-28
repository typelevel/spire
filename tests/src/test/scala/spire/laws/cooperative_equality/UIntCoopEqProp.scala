package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.UInt

object UIntCoopEqProp extends Properties("UInt") {

	val genInt: Gen[UInt]  = arbitrary[Int].map(UInt(_))
	val genLong: Gen[UInt] = arbitrary[Long].map(UInt(_))

	val intProps: List[Prop]  = Utility.props[UInt](genInt)
	val longProps: List[Prop] = Utility.props[UInt](genLong)

	Utility.printPreTestRun(Utility.UINT, Utility.INT)
  	intProps.foreach(_.check)

	Utility.printPreTestRun(Utility.UINT, Utility.LONG)
  	longProps.foreach(_.check)
}