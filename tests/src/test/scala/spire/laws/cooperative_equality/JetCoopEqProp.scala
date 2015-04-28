package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.{Jet, JetDim}
import spire.implicits._

object JetCoopEqProp extends Properties("Jet") {

	implicit val jetDim: JetDim = JetDim(23) // 23 is arbitrary

	// val genInt: Gen[Jet[Int]]       = arbitrary[Int].map(Jet(_))
	// val genLong: Gen[Jet[Long]]     = arbitrary[Long].map(Jet(_))
	val genDouble: Gen[Jet[Double]] = arbitrary[Double].map(Jet(_))
	val genFloat: Gen[Jet[Float]]   = arbitrary[Float].map(Jet(_))	
	// val genBigInt: Gen[Jet[BigInt]] = arbitrary[BigInt].map(Jet(_))

	// val intProps: List[Prop]    = Utility.props[Jet[Int]](genInt)
	val doubleProps: List[Prop] = Utility.props[Jet[Double]](genDouble)
	// val longProps: List[Prop]   = Utility.props[Jet[Long]](genLong)
	// val bigIntProps: List[Prop] = Utility.props[Jet[BigInt]](genBigInt)
	val floatProps: List[Prop]  = Utility.props[Jet[Float]](genFloat)

  	Utility.printPreTestRun(Utility.JET, Utility.FLOAT)
  	floatProps.foreach(_.check)

  	Utility.printPreTestRun(Utility.JET, Utility.DOUBLE)
  	doubleProps.foreach(_.check)  	
}