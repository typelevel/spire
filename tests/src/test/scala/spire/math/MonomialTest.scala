package spire.math

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag
import spire.algebra._
import spire.math.poly._
import spire.util.Opt

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalatest.Matchers
import org.scalatest._
import prop._
import Gen._
import Arbitrary.arbitrary

object MonomialSetup {

  implicit def arbitraryVariable = Arbitrary(for {
    c <- Gen.choose('x', 'z')
    v <- Gen.choose(1, 10)
  } yield (c, v))

}

class MonomialTest extends FunSuite {

  def assertDescending(strings: String*)(implicit order: Order[Monomial]): Unit = {
    import spire.syntax.order._
    val descendingSeq = strings.map(Monomial.parse(_))
    (descendingSeq zip descendingSeq.tail).foreach { case (m1, m2) =>
      assert(m1 > m2)
    }
  }

  test("Lexicographic ordering") {
    assertDescending("x^2", "xy", "xz", "x", "y^2", "yz", "y", "z^2", "z", "")(MonomialLexOrder)
  }

  test("Graded lexicographic ordering") {
    assertDescending("x^2", "xy", "xz", "y^2", "yz", "z^2", "x", "y", "z", "")(MonomialGradLexOrder)
  }

  test("Graded reverse lexicographic ordering") {
    assertDescending("x^2", "xy", "y^2", "xz", "yz", "z^2", "x", "y", "z", "")(MonomialGradRevLexOrder)
  }

  // For these examples, see http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-972-algebraic-techniques-and-semidefinite-optimization-spring-2006/lecture-notes/lecture_14.pdf
  test("Pablo Parillo example 1") {
    assertDescending("x^2y^2","x^2y","x^2","xy^2","xy","x","y^2","y","1")(MonomialLexOrder)
  }

  test("Pablo Parillo example 2") {
    assertDescending("x^3","x^2y","xy^2","y^3","x^2","xy","y^2","x","y", "1")(MonomialGradLexOrder)
  }

  test("Pablo Parillo example 3") {
    assertDescending("x^3","x^2y","xy^2","y^3","x^2","xy","y^2","x","y", "1")(MonomialGradRevLexOrder)
  }

  val alpha = Monomial.parse("x^3y^2z^8")
  val beta = Monomial.parse("x^2y^9z^2")

  test("Pablo Parillo example 4") {
    assert(MonomialLexOrder.compare(alpha, beta) == 1)
  }

  test("Pablo Parillo example 5") {
    assert(MonomialGradLexOrder.compare(alpha, beta) == 1)
  }

  test("Pablo Parillo example 6") {
    assert(MonomialGradRevLexOrder.compare(alpha, beta) == -1)
  }

}

class MonomialCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val arb: Arbitrary[Monomial] = Arbitrary(for {
    vs <- arbitrary[List[(Char,Int)]]
  } yield Monomial(vs) )

  val one = Monomial.empty

  type M = Monomial

  property("m = m") {
    forAll { (m: M) => m shouldBe m }
  }

  property("m * 1 = m") {
    forAll { (m: M) => m * one shouldBe m }
  }

  property("x * y = y * x") {
    forAll { (x: M, y: M) => x * y shouldBe y * x }
  }

  property("m * m = m.pow(2)") {
    forAll { (m: M) => m * m shouldBe m.pow(2) }
  }

  property("x divides (x * y)") {
    forAll { (x: M, y: M) => x.divides(x * y) shouldBe true }
  }

  property("m.isEmpty <=> m == 1") {
    forAll { (m: M) => m.isEmpty shouldBe (m == one) }
  }

  property("x divides x.lcm(y)") {
    forAll { (x: M, y: M) => x.divides(x.lcm(y)) shouldBe true }
  }

  property("x.gcf(y) divides x") {
    forAll { (x: M, y: M) => (x.gcf(y)).divides(x) shouldBe true }
  }

  property("x.gcf(y) * x.lcm(y) == x * y") {
    forAll { (x: M, y: M) => (x.gcf(y) * x.lcm(y)) shouldBe (x * y) }
  }

  property("(x * y) / x = y") {
    forAll { (x: M, y: M) =>
      val Opt(z) = (x * y)/x
      z shouldBe y
    }
  }

  property("m = Monomial((0 until m.nVariables) map { i => (m.variable(i), m.exponent(i)) })") {
    forAll { (m: M) => Monomial((0 until m.nVariables).map { i => (m.variable(i), m.exponent(i)) }) shouldBe m }
  }

  property("m = Monomial.parse(m.toString)") {
    forAll { (m: M) => Monomial.parse(m.toString) shouldBe m }
  }

}
