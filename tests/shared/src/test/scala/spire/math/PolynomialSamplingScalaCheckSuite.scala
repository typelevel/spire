package spire
package math

import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.syntax.euclideanRing._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import org.scalacheck.Prop._

class PolynomialSamplingScalaCheckSuite extends munit.ScalaCheckSuite {

  import PolynomialSetup._

  val ebd = Eq[BigDecimal]
  val fbd = Field[BigDecimal]
  val cbd = implicitly[ClassTag[BigDecimal]]

  runDense[Rational]("rational")
  runSparse[Rational]("rational")

  def runDense[A: Arbitrary: Eq: Field: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts.take(6)).toDense
    })
    runTest[A](s"$typ/dense")
  }

  def runSparse[A: Arbitrary: Eq: Field: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts.take(6)).toSparse
    })
    runTest[A](s"$typ/sparse")
  }

  def runTest[A: Eq: Field: ClassTag](
    name: String
  )(implicit arb: Arbitrary[Polynomial[A]], arb2: Arbitrary[A]): Unit = {
    type P = Polynomial[A]

    def testUnop(f: P => P)(g: A => A) = {
      forAll { (x: P, a: A) =>
        val z = f(x)
        g(x(a)) == z(a)
      }
    }

    def testBinop(f: (P, P) => P)(g: (A, A) => A) = {
      forAll { (x: P, y: P, a: A) =>
        val z = f(x, y)
        g(x(a), y(a)) == z(a)
      }
    }

    def testBinopNonzero(f: (P, P) => P)(g: (A, A) => A) = {
      forAll { (x: P, y: P, a: A) =>
        if (!y.isZero && y(a) != Field[A].zero) {
          val z = f(x, y)
          g(x(a), y(a)) == z(a)
        } else true
      }
    }

    property(s"$name unop -") { testUnop(-_)(-_) }
    property(s"$name unop pow(2)") { testUnop(_.pow(2))(_.pow(2)) }
    property(s"$name unop pow(3)") { testUnop(_.pow(3))(_.pow(3)) }

    property(s"$name binop +") { testBinop(_ + _)(_ + _) }
    property(s"$name binop -") { testBinop(_ - _)(_ - _) }
    property(s"$name binop *") { testBinop(_ * _)(_ * _) }
    property(s"$name binop /~ and %") {
      testBinopNonzero { (x, y) =>
        (x.equot(y)) * y + (x.emod(y))
      } { (a, b) =>
        (a.equot(b)) * b + (a.emod(b))
      }
    }
  }
}
