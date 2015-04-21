package spire.laws

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.{Algebraic, Rational, SafeLong}

/** 
 * Cooperative Equality = equals and hashCode agree
 */
object CooperativeEquality extends Properties("Algebraic") {

	def coopEquals[A](x: A, y: A) = x == y && x.hashCode == y.hashCode

	val genAlgebraicInt: Gen[Algebraic]                 = arbitrary[Int].map(Algebraic(_))
  val genAlgebraicLong: Gen[Algebraic]                = arbitrary[Long].map(Algebraic(_))
  val genAlgebraicDouble: Gen[Algebraic]              = arbitrary[Double].map(Algebraic(_))
  val genAlgebraicBigInt: Gen[Algebraic]              = arbitrary[BigInt].map(Algebraic(_))
  val genSafeLong: Gen[SafeLong]                      = arbitrary[Long].map(SafeLong(_))
  implicit val arbitrarySafeLong: Arbitrary[SafeLong] = Arbitrary(genSafeLong)
  val genAlgebraicRational: Gen[Algebraic]            = for {
    num <- arbitrary[SafeLong]
    neg <- Gen.choose(Long.MinValue, -1)
    pos <- Gen.choose(1, Long.MaxValue)
    den <- Gen.oneOf(neg, pos)
  } yield Rational(num, den)
  val genAlgebraicBigDecimal: Gen[Algebraic] = arbitrary[BigDecimal].map(Algebraic(_))

  val intProps: List[Prop]        = algebraicProps(genAlgebraicInt)
  val longProps: List[Prop]       = algebraicProps(genAlgebraicLong)
  val doubleProps: List[Prop]     = algebraicProps(genAlgebraicDouble)
  val bigIntProps: List[Prop]     = algebraicProps(genAlgebraicBigInt)
  val rationalProps: List[Prop]   = algebraicProps(genAlgebraicRational)
  val bigDecimalProps: List[Prop] = algebraicProps(genAlgebraicBigDecimal)

  printPreTestRun("Int")
  intProps.foreach(_.check)

  printPreTestRun("Long")
  longProps.foreach(_.check)  
  
  printPreTestRun("Double")
  doubleProps.foreach(_.check)
  
  printPreTestRun("BigInt")
  bigIntProps.foreach(_.check)

  printPreTestRun("Rational")
  rationalProps.foreach(_.check)

  // printPreTestRun("BigDecimal")
  // bigDecimalProps.foreach(_.check)

  
  def printPreTestRun(t: String): Unit = println(s"Checking Algebraic's Commutativity for + and *, as well as Transitivity, for $t")

  def algebraicProps(g: Gen[Algebraic]): List[Prop] = {
    val commutativeAdd = forAll(g, g) { (a: Algebraic, b: Algebraic) =>
      coopEquals(a+b,b+a) 
    }
    val commutativeMult = forAll(g, g) { (a: Algebraic, b: Algebraic) =>
      coopEquals(a*b,b*a) 
    }
    val transitive = forAll(g, g, g) { (a: Algebraic, b: Algebraic, c: Algebraic) =>
      if( coopEquals(a, b) && coopEquals(b, c)) coopEquals(a, c) else true
    }
    List(commutativeAdd, commutativeMult, transitive)
  }

}
