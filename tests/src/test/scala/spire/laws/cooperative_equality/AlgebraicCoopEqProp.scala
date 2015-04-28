package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Test.Parameters

import spire.math.{Algebraic, Rational, SafeLong}
import spire.laws.cooperative_equality.Utility.arbitrarySafeLong

object AlgebraicCoopEqProp extends Properties("Algebraic") {

	val genAlgebraicInt: Gen[Algebraic]                 = arbitrary[Int].map(Algebraic(_))
  val genAlgebraicLong: Gen[Algebraic]                = arbitrary[Long].map(Algebraic(_))
  val genAlgebraicDouble: Gen[Algebraic]              = arbitrary[Double].map(Algebraic(_))
  val genAlgebraicBigInt: Gen[Algebraic]              = arbitrary[BigInt].map(Algebraic(_))
  val genAlgebraicRational: Gen[Algebraic]            = for {
    num <- arbitrary[SafeLong]
    neg <- Gen.choose(Long.MinValue, -1)
    pos <- Gen.choose(1, Long.MaxValue)
    den <- Gen.oneOf(neg, pos)
  } yield Algebraic(Rational(num, den))
  val genAlgebraicBigDecimal: Gen[Algebraic] = arbitrary[BigDecimal].map(Algebraic(_))

  val intProps: List[Prop]        = Utility.props[Algebraic](genAlgebraicInt)
  val longProps: List[Prop]       = Utility.props[Algebraic](genAlgebraicLong)
  val doubleProps: List[Prop]     = Utility.props[Algebraic](genAlgebraicDouble)
  val bigIntProps: List[Prop]     = Utility.props[Algebraic](genAlgebraicBigInt)
  val rationalProps: List[Prop]   = Utility.props[Algebraic](genAlgebraicRational)
  val bigDecimalProps: List[Prop] = Utility.props[Algebraic](genAlgebraicBigDecimal)

  Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.INT)
  intProps.foreach(_.check)

  Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.LONG)
  longProps.foreach(_.check)  
  
  Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.DOUBLE)
  doubleProps.foreach(_.check)
  
  Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.BIG_INT)
  bigIntProps.foreach(_.check)

  Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.RATIONAL)
  rationalProps.foreach(_.check)

  // ERROR: failed to terminate after ~1 hour
  // Utility.printPreTestRun(Utility.ALGEBRAIC, Utility.BIG_DECIMAL)
  // bigDecimalProps.foreach(_.check(Parameters.default.withMinSuccessfulTests(10)))

}