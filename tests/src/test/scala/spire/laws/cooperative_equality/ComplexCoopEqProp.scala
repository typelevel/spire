package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.{Complex, Rational, SafeLong}
import spire.math.Complex._
import spire.implicits._

import spire.laws.cooperative_equality.Utility.arbitraryRational

/** 
 * Cooperative Equality for Complex.
 */
object ComplexCoopEqProp extends Properties("Complex") {

	val genComplexInt: Gen[Complex[Int]] = for {
		x <- arbitrary[Int]
		y <- arbitrary[Int]
	} yield Complex(x, y)

	val genComplexLong: Gen[Complex[Long]] = for {
		x <- arbitrary[Long]
		y <- arbitrary[Long]
	} yield Complex(x, y)	

	val genComplexDouble: Gen[Complex[Double]] = for {
		x <- arbitrary[Double]
		y <- arbitrary[Double]
	} yield Complex(x, y)

	val genComplexBigInt: Gen[Complex[BigInt]] = for {
		x <- arbitrary[BigInt]
		y <- arbitrary[BigInt]
	} yield Complex(x, y)		

	val genComplexRational: Gen[Complex[Rational]] = for {
		x <- arbitrary[Rational]
		y <- arbitrary[Rational]
	} yield Complex(x, y)		

	val genComplexBigDecimal: Gen[Complex[BigDecimal]] = for {
		x <- arbitrary[BigDecimal]
		y <- arbitrary[BigDecimal]
	} yield Complex(x, y)					

	val intProps: List[Prop]    = Utility.props[Complex[Int]](genComplexInt)
	val doubleProps: List[Prop] = Utility.props[Complex[Double]](genComplexDouble)
	val longProps: List[Prop]   = Utility.props[Complex[Long]](genComplexLong)
	val bigIntProps: List[Prop]   = Utility.props[Complex[BigInt]](genComplexBigInt)
	val rationalProps: List[Prop]   = Utility.props[Complex[Rational]](genComplexRational)
	val bigDecimalProps: List[Prop]   = Utility.props[Complex[BigDecimal]](genComplexBigDecimal)

  	Utility.printPreTestRun(Utility.COMPLEX, Utility.INT)
  	intProps.foreach(_.check)

  	Utility.printPreTestRun(Utility.COMPLEX, Utility.DOUBLE)
  	doubleProps.foreach(_.check)  	

  	Utility.printPreTestRun(Utility.COMPLEX, Utility.LONG)
  	longProps.foreach(_.check)  	

  	Utility.printPreTestRun(Utility.COMPLEX, Utility.BIG_INT)
  	bigIntProps.foreach(_.check)  	

  	Utility.printPreTestRun(Utility.COMPLEX, Utility.RATIONAL)
  	rationalProps.foreach(_.check)  

}
