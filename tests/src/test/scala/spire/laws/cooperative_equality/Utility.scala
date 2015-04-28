package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.algebra.Ring
import spire.math.{SafeLong, Rational}
import spire.implicits._

object Utility {

  def coopEquals[A](x: A, y: A): Boolean = x == y && x.hashCode == y.hashCode

  val ALGEBRAIC = "Algebraic"
  val COMPLEX   = "Complex"
  val JET       = "Jet"
  val NATURAL   = "Natural"
  val NUMBER    = "Number"

  val INT         = "Int"
  val LONG 	      = "Long"
  val DOUBLE      = "Double"
  val FLOAT       = "Float"
  val BIG_INT     = "BigInt"
  val RATIONAL    = "Rational"
  val BIG_DECIMAL = "BigDecimal"
  val SAFE_LONG   = "SafeLong"

  val genSafeLong: Gen[SafeLong]                      = arbitrary[Long].map(SafeLong(_))
  implicit val arbitrarySafeLong: Arbitrary[SafeLong] = Arbitrary(genSafeLong)

  def props[A : Ring](g: Gen[A]): List[Prop] = {
    val commutativeAdd = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a+b,b+a) 
    }
    val commutativeMult = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a*b,b*a) 
    }
    val transitive = forAll(g, g, g) { (a: A, b: A, c: A) =>
      if( coopEquals(a, b) && coopEquals(b, c)) coopEquals(a, c) else true
    }
    List(coopEqProp(g), commutativeAdd, commutativeMult, transitive)
  }

  def coopEqProp[A](g: Gen[A]): Prop = 
    forAll(g) { (a: A) => coopEquals(a, a) }

  val genRational: Gen[Rational] = for {
  	num <- arbitrary[SafeLong]
  	neg <- Gen.choose(Long.MinValue, -1)
    pos <- Gen.choose(1, Long.MaxValue)
    den <- Gen.oneOf(neg, pos)
  } yield Rational(num, den)

  val genPositive: Gen[Int] = Gen.choose(1, Int.MaxValue)

  implicit val arbitraryRational: Arbitrary[Rational] = Arbitrary(genRational)

  def printPreTestRun(t: String, primitive: String): Unit = 
  	println(s"Checking $t's (1) Cooperative Equality, Commutativity for (2) + and (3) *, as well as (4) Transitivity, for $primitive")
}