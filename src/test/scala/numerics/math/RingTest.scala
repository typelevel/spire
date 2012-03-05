package numerics.math

// scalatest
import org.scalatest.FunSuite

// we need to disable our own === to avoid messing up ScalaTest.
import Implicits.{eqOps => _, _}

// nice alias
import scala.{specialized => spec}

import java.math.MathContext

import numerics.math.fpf.FPFilter


class RingTest extends FunSuite {

  /**
   * We use this function to avoid duplicating our tests for all the different
   * A's that we want to test. We expect the actual values to be:
   *
   *   a=-3  b=3  c=9
   */
  def runWith[@spec A:Ring:Manifest](a:A, b:A, c:A) {

    val m = implicitly[Manifest[A]]

    // the name to use for this A
    val cls = m.typeArguments match {
      case Nil => m.erasure.getSimpleName
      case args => "%s[%s]" format (m.erasure.getSimpleName, args.mkString(","))
    }

    // test runner which constructs a unique name for each test we run.
    def runTest(name:String)(f: => Unit) = test("%s:%s" format(cls, name))(f)

    // Ring[A]'s zero
    val z: A = Ring[A].zero

    // abs
    runTest("(-3).abs")(assert(a.abs === b))
    runTest("3.abs")(assert(b.abs === b))

    // unary_-
    runTest("-(3)")(assert(-b === a))
    runTest("-(0)")(assert(-z === z))

    // +
    runTest("3 + 0")(assert(b + z === b))
    runTest("3 + (-3)")(assert(b + a === z))

    // -
    runTest("3 - 0")(assert(b - z === b))
    runTest("3 - 3)")(assert(b - b === z))
    runTest("0 - 3)")(assert(z - b === a))

    // *
    runTest("3 * 0")(assert(b * z === z))
    runTest("3 * (-3)")(assert(b * a === c))

    // toInt
    runTest("3.toInt")(assert(b.toInt === 3))
  }

  implicit val mc: MathContext = MathContext.DECIMAL128

  // here's where we actually run all the tests, for each type we care about.
  runWith[Int](-3, 3, -9)
  runWith[Long](-3, 3, -9)
  runWith[Float](-3, 3, -9)
  runWith[Double](-3, 3, -9)
  runWith[BigInt](-3, 3, -9)
  runWith[BigDecimal](-3, 3, -9)
  runWith[Rational](-3, 3, -9)
  // commented out due to specialization bug
  runWith[Complex[Double]](-3, 3, -9)
  runWith[Complex[BigDecimal]](Complex(BigDecimal(-3), BigDecimal(0)),
                               Complex(BigDecimal(3), BigDecimal(0)),
                               Complex(BigDecimal(-9), BigDecimal(0)))
  runWith[FPFilter[BigInt]](FPFilter[BigInt](-3), FPFilter[BigInt](3), FPFilter[BigInt](-9))
}
