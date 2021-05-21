package spire
package algebra

// we need to disable our own === to avoid messing up ScalaTest.
import spire.math.{Complex, Jet, JetDim, Rational}
import spire.implicits.{eqOps => _, _}

// nice alias

import java.math.MathContext

class RingSuite extends munit.FunSuite {

  /**
   * We use this function to avoid duplicating our tests for all the different
   * A's that we want to test. We expect the actual values to be:
   *
   *   a=-3  b=3  c=-9
   */
  def runWith[@sp A: Ring: ClassTag](cls: String)(a: A, b: A, c: A): Unit = {

    //// the name to use for this A
    //val cls = m.typeArguments match {
    //  case Nil => m.erasure.getSimpleName
    //  case args => "%s[%s]" format (m.erasure.getSimpleName, args.mkString(","))
    //}

    // the name to use for this A
    //val cls = m.runtimeClass.getName

    // test runner which constructs a unique name for each test we run.
    def runTest(name: String)(f: => Unit) = test("%s:%s".format(cls, name))(f)

    // Ring[A]'s zero
    val z: A = Ring[A].zero

    // unary_-
    runTest("-(3)")(assertEquals(-b, a))
    runTest("-(0)")(assertEquals(-z, z))

    // +
    runTest("3 + 0")(assertEquals(b + z, b))
    runTest("3 + (-3)")(assertEquals(b + a, z))

    // -
    //implicit def fooby[B:Ring](b:B) = new AdditiveGroupOps[B](b)
    //runTest("3 - 0")(assertEquals(additiveGroupOps(b).-(z), b))
    runTest("3 - 0")(assertEquals(b - z, b))
    runTest("3 - 3)")(assertEquals(b - b, z))
    runTest("0 - 3)")(assertEquals(z - b, a))

    // *
    runTest("3 * 0")(assertEquals(b * z, z))
    runTest("3 * (-3)")(assertEquals(b * a, c))

    runTest("fromInt(3)")(assert(Ring[A].fromInt(3) == b))

    runTest("3 pow 2")(assertEquals((b.pow(2)), -c))
  }

  implicit val mc: MathContext = MathContext.DECIMAL128
  implicit val jetDim: JetDim = JetDim(7)

  // here's where we actually run all the tests, for each type we care about.
  runWith[Int]("Int")(-3, 3, -9)
  runWith[Long]("Long")(-3, 3, -9)
  runWith[Float]("Float")(-3, 3, -9)
  runWith[Double]("Double")(-3, 3, -9)
  runWith[BigInt]("BigInt")(-3, 3, -9)
  runWith[BigDecimal]("BigDecimal")(-3, 3, -9)
  runWith[Rational]("Rational")(-3, 3, -9)
  // commented out due to specialization bug
  runWith[Complex[Double]]("Complex[Double]")(-3, 3, -9)
  runWith[Complex[BigDecimal]]("Complex[BigDecimal]")(Complex(BigDecimal(-3), BigDecimal(0)),
                                                      Complex(BigDecimal(3), BigDecimal(0)),
                                                      Complex(BigDecimal(-9), BigDecimal(0))
  )
  runWith[Jet[Double]]("Jet[Double]")(Jet(-3), Jet(3), Jet(-9))

  {
    class XRing extends Ring[String] {
      def toX(n: Int) = if (n > 0) "x" * n else "-" + "x" * -n
      def fromX(s: String) = if (s.startsWith("-")) -(s.length - 1) else s.length

      private def unop(s: String)(f: Int => Int): String = toX(f(fromX(s)))
      private def binop(s1: String, s2: String)(f: (Int, Int) => Int): String = toX(f(fromX(s1), fromX(s2)))

      def negate(a: String) = unop(a)(-_)
      def one = "x"
      def plus(a: String, b: String) = binop(a, b)(_ + _)
      def times(a: String, b: String) = binop(a, b)(_ * _)
      def zero = ""
    }

    def x(n: Int) = xIsRing.fromInt(n)

    implicit object xIsRing extends XRing

    test("fromInt(-12)") { assertEquals(xIsRing.fromInt(-12), x(-12)) }
    test("fromInt(0)") { assertEquals(xIsRing.fromInt(0), x(0)) }
    test("fromInt(33)") { assertEquals(xIsRing.fromInt(33), x(33)) }

    test("2 ** 0") { assertEquals(x(2) ** 0, x(1)) }
    test("2 ** 1") { assertEquals(x(2) ** 1, x(2)) }
    test("2 ** 2") { assertEquals(x(2) ** 2, x(4)) }
    test("2 ** 3") { assertEquals(x(2) ** 3, x(8)) }
    test("2 ** 4") { assertEquals(x(2) ** 4, x(16)) }

    test("3 ** 0") { assertEquals(x(3) ** 0, x(1)) }
    test("3 ** 1") { assertEquals(x(3) ** 1, x(3)) }
    test("3 ** 2") { assertEquals(x(3) ** 2, x(9)) }
    test("3 ** 3") { assertEquals(x(3) ** 3, x(27)) }
    test("3 ** 4") { assertEquals(x(3) ** 4, x(81)) }
  }
}
