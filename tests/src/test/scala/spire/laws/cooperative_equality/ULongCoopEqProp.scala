package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.ULong

object ULongCoopEqProp extends Properties("ULong") {

	val genLong: Gen[ULong]   = Gen.choose(0, Long.MaxValue).map(ULong(_))
	val genString: Gen[ULong] = Gen.choose(0, Long.MaxValue).map(l => ULong(l.toString))

	val longProps: List[Prop]   = Utility.props[ULong](genLong)
	val stringProps: List[Prop] = Utility.props[ULong](genString)

	Utility.printPreTestRun(Utility.ULONG, Utility.LONG)
  	longProps.foreach(_.check)

	Utility.printPreTestRun(Utility.ULONG, Utility.STRING)
  	stringProps.foreach(_.check)
}
