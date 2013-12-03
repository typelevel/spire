package spire.math

import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.syntax.literals._
import spire.syntax.euclideanRing._
import spire.optional.rationalTrig._

import scala.reflect.ClassTag

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class PolynomialSamplingCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import PolynomialSetup._

  val ebd = Eq[BigDecimal]
  val fbd = Field[BigDecimal]
  val cbd = implicitly[ClassTag[BigDecimal]]
 
  runDense[Rational]("rational")
  runSparse[Rational]("rational")

  def runDense[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts).toDense
    })
    runTest[A](s"$typ/dense")
  }

  def runSparse[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts).toSparse
    })
    runTest[A](s"$typ/sparse")
  }

  def runTest[A: Eq: Field: ClassTag](name: String)(implicit arb: Arbitrary[Polynomial[A]], arb2: Arbitrary[A]) {
    type P = Polynomial[A]

    val zero = Polynomial.zero[A]
    val one = Polynomial.one[A]

    def testUnop(f: P => P)(g: A => A) {
      forAll { (x: P, a: A) =>
        val z = f(x)
        g(x(a)) shouldBe z(a)
      }
    }

    def testBinop(f: (P, P) => P)(g: (A, A) => A) {
      forAll { (x: P, y: P, a: A) =>
        val z = f(x, y)
        g(x(a), y(a)) shouldBe z(a)
      }
    }

    def testBinopNonzero(f: (P, P) => P)(g: (A, A) => A) {
      forAll { (x: P, y: P, a: A) =>
        if (!y.isZero && a != Field[A].zero) {
          val z = f(x, y)
          g(x(a), y(a)) shouldBe z(a)
        }
      }
    }

    property(s"$name unop -") { testUnop(-_)(-_) }
    property(s"$name unop pow(2)") { testUnop(_.pow(2))(_.pow(2)) }
    property(s"$name unop pow(3)") { testUnop(_.pow(3))(_.pow(3)) }

    property(s"$name binop +") { testBinop(_ + _)(_ + _) }
    property(s"$name binop -") { testBinop(_ - _)(_ - _) }
    property(s"$name binop *") { testBinop(_ * _)(_ * _) }
    property(s"$name binop /~ and %") {
      testBinopNonzero({ (x, y) =>
        (x /~ y) * y + (x % y)
      })({ (a, b) =>
        (a /~ b) * b + (a % b)
      })
    }
  }
}
