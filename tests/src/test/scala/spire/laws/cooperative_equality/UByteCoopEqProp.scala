package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.UByte

object UByteCoopEqProp extends Properties("UByte") {
	
	val genByte: Gen[UByte] = arbitrary[Byte].map(UByte(_))
	val genInt: Gen[UByte] = arbitrary[Int].map(UByte(_))

	val byteProps: List[Prop] = Utility.props[UByte](genByte)
	val intProps: List[Prop] = Utility.props[UByte](genInt)

	Utility.printPreTestRun(Utility.UBYTE, Utility.BYTE)
  	byteProps.foreach(_.check)

	Utility.printPreTestRun(Utility.UBYTE, Utility.INT)
  	intProps.foreach(_.check)  	

}