package spire.laws

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Algebraic

/** 
 * Cooperative Equality = equals and hashCode agree
 */
object CooperativeEquality extends Properties("Algebraic") {

	def coopEquals[A](x: A, y: A) = x == y && x.hashCode == y.hashCode

	val genAlgebraicInt: Gen[Algebraic]      = arbitrary[Int].map(Algebraic(_))
  val genAlgebraicDouble: Gen[Algebraic]   = arbitrary[Double].map(Algebraic(_))
  val genAlgebraicBigInt: Gen[Algebraic]   = arbitrary[Double].map(Algebraic(_))
  //val genAlgebraicRational: Gen[Algebraic] = arbitrary[Rational].map(Algebraic(_))

  val intProps: List[Prop]    = algebraicProps(genAlgebraicInt)
  val doubleProps: List[Prop] = algebraicProps(genAlgebraicDouble)
  val bigIntProps: List[Prop] = algebraicProps(genAlgebraicBigInt)

  println("Checking Algebraic's Commutativity for Addition and Multiplicate with Int")
  intProps.foreach(_.check)
  println("Checking Algebraic's Commutativity for Addition and Multiplicate with Double")
  doubleProps.foreach(_.check)
  println("Checking Algebraic's Commutativity for Addition and Multiplicate with BigInt")
  bigIntProps.foreach(_.check)

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