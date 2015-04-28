package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.UShort

object UShortCoopEqProp extends Properties("UShort") {

	val genChar: Gen[UShort]  = arbitrary[Char].map(UShort(_))
	val genShort: Gen[UShort] = arbitrary[Short].map(UShort(_))
	val genInt: Gen[UShort]   = arbitrary[Int].map(UShort(_))

	val charProps: List[Prop]  = Utility.props[UShort](genChar)
	val shortProps: List[Prop] = Utility.props[UShort](genShort)
	val intProps: List[Prop]   = Utility.props[UShort](genInt)

	Utility.printPreTestRun(Utility.USHORT, Utility.CHAR)
  	charProps.foreach(_.check)

	Utility.printPreTestRun(Utility.USHORT, Utility.SHORT)
  	shortProps.foreach(_.check)

	Utility.printPreTestRun(Utility.USHORT, Utility.INT)
  	intProps.foreach(_.check)

}