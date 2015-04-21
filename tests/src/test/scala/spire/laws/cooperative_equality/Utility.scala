package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.algebra.Rig
import spire.math.{SafeLong, Rational}
import spire.implicits._

object Utility {

  def coopEquals[A](x: A, y: A): Boolean = x == y && x.hashCode == y.hashCode

  val ALGEBRAIC = "Algebraic"
  val COMPLEX   = "Complex"

  val INT         = "Int"
  val LONG 	      = "Long"
  val DOUBLE      = "Double"
  val BIG_INT     = "BigInt"
  val RATIONAL    = "Rational"
  val BIG_DECIMAL = "BigDecimal"

  val genSafeLong: Gen[SafeLong]                      = arbitrary[Long].map(SafeLong(_))
  implicit val arbitrarySafeLong: Arbitrary[SafeLong] = Arbitrary(genSafeLong)

  def props[A : Rig](g: Gen[A]): List[Prop] = {
    val commutativeAdd = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a+b,b+a) 
    }
    val commutativeMult = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a*b,b*a) 
    }
    val transitive = forAll(g, g, g) { (a: A, b: A, c: A) =>
      if( coopEquals(a, b) && coopEquals(b, c)) coopEquals(a, c) else true
    }
    List(commutativeAdd, commutativeMult, transitive)
  }

  val genRational: Gen[Rational] = for {
  	num <- arbitrary[SafeLong]
  	neg <- Gen.choose(Long.MinValue, -1)
    pos <- Gen.choose(1, Long.MaxValue)
    den <- Gen.oneOf(neg, pos)
  } yield Rational(num, den)

  implicit val arbitraryRational: Arbitrary[Rational] = Arbitrary(genRational)

  def printPreTestRun(t: String, primitive: String): Unit = 
  	println(s"Checking $t's Commutativity for + and *, as well as Transitivity, for $primitive")
}