package spire
package math

import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuite
import spire.algebra._
import spire.implicits._

class JetTest extends AnyFunSuite with Matchers {

  // Default test with 3-dimensional Jet's
  implicit val dim = JetDim(3)
  val maxError = 1.0e-12

  test("JetDim") {
    dim.dimension should equal (3)
  }

  test("Jet(scalar, array) constructor") {
    val a = Array[Double](2.3, 3.4, 4.5)
    val j = new Jet[Double](8.9, a)
    j.dimension should equal (dim.dimension)
    j.jetDimension.dimension should equal (dim.dimension)
    j.infinitesimal.size should equal (dim.dimension)
    j.real should equal (8.9)
    j.infinitesimal.toArray should equal (a.toArray)
  }
  test("Jet() constructor yields a zero jet") {
    val jz = Jet[Double]()
    jz.real should equal (0)
    jz.isReal should be (true)
    jz.infinitesimal.size should equal (dim.dimension)
    jz.isInfinitesimal should be (false)
    jz.isZero should be (true)
  }
  test("Jet.zero yields a zero jet") {
    val jzz = Jet.zero[Double]
    jzz.real should equal (0)
    jzz.isReal should be (true)
    jzz.infinitesimal.size should equal (dim.dimension)
    jzz.isInfinitesimal should be (false)
    jzz.isZero should be (true)
  }
  test("Jet.one yields a unitary jet") {
    val jo = Jet.one[Double]
    jo.real should equal (1.0)
    jo.isReal should be (true)
    jo.infinitesimal.size should equal (dim.dimension)
    jo.isInfinitesimal should be (false)
  }
  test("Jet.h yields an infinitesimal jet") {
    val jk = Jet.h[Double](k = 1)
    jk.real should equal (0.0)
    jk.isReal should be (false)
    jk.isInfinitesimal should be (true)
    jk.infinitesimal.toArray should equal (Array(0.0, 1.0, 0.0))
  }
  test("Jet(x, k) yields a jet for evaluating a function and its k-th partial derivative") {
    val jk = Jet(2.3, k = 2)
    jk.real should equal (2.3)
    jk.isReal should be (false)
    jk.isInfinitesimal should be (false)
    jk.infinitesimal.toArray should equal (Array(0.0, 0.0, 1.0))
  }
  test("Jet(x) constructors from scalars") {
    val jf = Jet(2.0f)
    jf.real should equal (2.0f)
    jf.isReal should be (true)
    jf.infinitesimal.size should equal (dim.dimension)
    jf.isInfinitesimal should be (false)

    val jd = Jet(2.6)
    jd.real should equal (2.6)
    jd.isReal should be (true)
    jd.infinitesimal.size should equal (dim.dimension)
    jd.isInfinitesimal should be (false)

    val jbd = Jet(BigDecimal(2847.694984))
    jbd.real should equal (BigDecimal(2847.694984))
    jbd.isReal should be (true)
    jbd.infinitesimal.size should equal (dim.dimension)
    jbd.isInfinitesimal should be (false)

    val jfi = Jet.fromInt[Float](2)
    jfi.real should equal (2.0)
    jfi.isReal should be (true)
    jfi.infinitesimal.size should equal (dim.dimension)
    jfi.isInfinitesimal should be (false)
  }
  test("Conversions from scalars") {
    val jfi = Jet.intToJet(2)
    jfi.real should equal (2.0)
    jfi.isReal should be (true)
    jfi.infinitesimal.size should equal (dim.dimension)
    jfi.isInfinitesimal should be (false)

    val jfl = Jet.longToJet(2L)
    jfl.real should equal (2.0)
    jfl.isReal should be (true)
    jfl.infinitesimal.size should equal (dim.dimension)
    jfl.isInfinitesimal should be (false)

    val jff = Jet.floatToJet(2.47f)
    jff.real should equal (2.47f)
    jff.isReal should be (true)
    jff.infinitesimal.size should equal (dim.dimension)
    jff.isInfinitesimal should be (false)

    val jfd = Jet.doubleToJet(2.47)
    jfd.real should equal (2.47)
    jfd.isReal should be (true)
    jfd.infinitesimal.size should equal (dim.dimension)
    jfd.isInfinitesimal should be (false)

    val jfbi = Jet.bigIntToJet(BigInt(247847))
    jfbi.real should equal (BigDecimal(247847))
    jfbi.isReal should be (true)
    jfbi.infinitesimal.size should equal (dim.dimension)
    jfbi.isInfinitesimal should be (false)

    val jfbd = Jet.bigDecimalToJet(BigDecimal(247847.28375))
    jfbd.real should equal (BigDecimal(247847.28375))
    jfbd.isReal should be (true)
    jfbd.infinitesimal.size should equal (dim.dimension)
    jfbd.isInfinitesimal should be (false)
  }
  test("Conversions to scalars") {
    val j = Jet(8.7, Array(7.97, 9.31, 0.0))
    j.doubleValue should equal (8.7)
    j.floatValue should equal (8.7f)
    j.longValue should equal (8L)
    j.intValue should equal (8)
    j.shortValue should equal (8.toShort)
    j.byteValue should equal (8.toByte)
  }
  test("Conversion to tuple") {
    val j = Jet(-3.1, Array(1.0, 2.1, 3.3))
    val tj = j.asTuple
    tj._1 should be(j.real)
    tj._2 should be(j.infinitesimal)
  }
  test("Signed") {
    Jet(9.1).signum should equal (1)
    Jet(-3.1, Array(1.0, 2.1, 3.3)).signum should equal (-1)
  }
  test("isWhole iff real and integer") {
    Jet(4.0f).isWhole should be (true)
    Jet(9.2).isWhole should be (false)
    Jet(3.0f, Array(1.0f, 2.0f, 3.0f)).isWhole should be (false)
  }
  test("isValidInt iff real, whole and in range") {
    Jet(-4.0).isValidInt should be (true)
    Jet(4.1).isValidInt should be (false)
    Jet(3.0f, Array(1.0f, 2.0f, 3.0f)).isValidInt should be (false)
    Jet(Int.MinValue.toDouble - 1000.0).isValidInt should be (false)
    Jet(Int.MaxValue.toDouble + 1000.0).isValidInt should be (false)
  }
  test("Equality-comparable and hashable") {
    val r = 13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    val b = Jet(r.toDouble, i.map(_.toDouble))
    val c = Jet(b.real.toFloat, b.infinitesimal.map(_.toFloat))
    // Value-based, symmetric, reflexive, transitive
    a should equal (a)
    a should equal (b)
    b should equal (a)
    b should equal (c)
    a should equal (c)
    // Same for eqv
    val bf: Jet[Float] = a.copy()
    val cf: Jet[Float] = a.copy()
    a eqv a should be (true)
    a eqv bf should be (true)
    bf eqv a should be (true)
    bf eqv cf should be (true)
    a eqv cf should be (true)
    // Reverse for neqv
    a neqv a should be (false)
    a neqv bf should be (false)
    bf neqv a should be (false)
    bf neqv cf should be (false)
    a neqv cf should be (false)
    // Likewise for hashes
    a.hashCode should equal (a.hashCode)
    a.hashCode should equal (b.hashCode)
    b.hashCode should equal (a.hashCode)
    b.hashCode should equal (c.hashCode)
    a.hashCode should equal (c.hashCode)
  }
  test("Prints into a human-readable string") {
    Jet(2.4, k = 2).toString should equal ("(2.4 + [0.0, 0.0, 1.0]h)")
  }
  test("Unary minus operator") {
    val r = -13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    -a should equal (new Jet(-r, -i))
    -(-a) should equal (a)
  }
  test("Arithmetic combinations with scalars") {
    val r = 13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    val b = 97.0f
    (a + b) should equal (a.copy(real = r + b))
    (a - b) should equal (a.copy(real = r - b))
    (a * b) should equal (a.copy(real = r * b, infinitesimal = i :* b))
    (a / b) should equal (a.copy(real = r / b, infinitesimal = i :/ b))
  }
  test("Arithmetic combinations with scalar and non-scalar Jets") {
    val a = Jet(1.0, Array(2.0, 3.0, 4.0))
    val b = Jet(2.0)
    (a + b) should equal (Jet(a.real + b.real, a.infinitesimal))
    (a - b) should equal (Jet(a.real - b.real, a.infinitesimal))
    (a * b) should equal (Jet(a.real * b.real, a.infinitesimal :* b.real))
    (a / b) should equal (Jet(a.real / b.real, a.infinitesimal :/ b.real))
  }
  test("Evaluation and differentiation of arithmetic operations") {
    def a[@sp(Double) T : Field : Trig](x: T): T = 2.0 * x + spire.math.cos[T](x)
    def b[@sp(Double) T : Field : Trig](x: T): T = spire.math.sin(x) - spire.math.log[T](x) + 7.3
    def da(x: Double) = 2.0 - scala.math.sin(x)
    def db(x: Double) = scala.math.cos(x) - 1/x

    val x = 894.3287562
    val jx = x + Jet.h[Double](0)

    def a_plus_b[@sp(Double) T : Field : Trig](x: T): T = a(x) + b(x)
    a_plus_b(jx).real should be(a(x) + b(x))
    a_plus_b(jx).infinitesimal(0) should be(da(x) + db(x) +- maxError)

    def a_minus_b[@sp(Double) T : Field : Trig](x: T): T = a(x) - b(x)
    a_minus_b(jx).real should be(a(x) - b(x))
    a_minus_b(jx).infinitesimal(0) should be(da(x) - db(x) +- maxError)

    def a_times_b[@sp(Double) T : Field : Trig](x: T): T = a(x) * b(x)
    a_times_b(jx).real should be(a(x) * b(x))
    a_times_b(jx).infinitesimal(0) should be(da(x) * b(x) + a(x) * db(x) +- maxError)

    def a_div_b[@sp(Double) T : Field : Trig](x: T): T = a(x) / b(x)
    a_div_b(jx).real should be(a(x) / b(x))
    a_div_b(jx).infinitesimal(0) should be((da(x) * b(x) - a(x) * db(x)) / (b(x) * b(x)) +- maxError)
  }
  test("Evaluation and differentiation of exponential and logarithm") {
    val x = 27.98847750
    val jx = x + Jet.h[Double](0)
    val lx = spire.math.log(jx)
    lx.real should be(scala.math.log(x))
    lx.infinitesimal(0) should be(1.0 / x +- maxError)

    val ex = spire.math.exp(jx)
    ex.real should be(scala.math.exp(x))
    ex.infinitesimal(0) should be (scala.math.exp(x) +- maxError)
  }
  test("Evaluation and differentiation of powers and roots") {
    // Constant integer exponent: x ** n
    val x = 9.3874983
    val jx = x + Jet.h[Double](0)
    val n = 5
    val jxn = jx.pow(n)
    jxn.real should be(scala.math.pow(x, n) +- maxError)
    jxn.infinitesimal(0) should be(n * scala.math.pow(x, n - 1) +- maxError)
    // Constant double exponent: x ** d
    val d = 0.387
    val jxd = jx.pow(d)
    jxd.real should be(scala.math.pow(x, d))
    jxd.infinitesimal(0) should be(scala.math.exp(d * scala.math.log(x)) * d / x +- maxError)
    // Variable base and exponent: sin(x) ** x
    val jex = spire.math.sin(jx)
    val jp = jx.pow(jex)
    jp.real should be(scala.math.pow(x, scala.math.sin(x)) +- maxError)
    jp.infinitesimal(0) should be(scala.math.exp(scala.math.sin(x) * scala.math.log(x)) *
      (scala.math.cos(x) * scala.math.log(x) + scala.math.sin(x) / x) +- maxError)
    // Square root
    val sq = spire.math.sqrt(jx)
    sq.real should be(scala.math.sqrt(x))
    sq.infinitesimal(0) should be(0.5 / scala.math.sqrt(x))
  }
  test("Evaluation and differentiation of trigonometric functions") {
    val x = 0.8377469
    val jx = x + Jet.h[Double](0)
    spire.math.cos(jx).real should be(scala.math.cos(x))
    spire.math.cos(jx).infinitesimal(0) should be(-scala.math.sin(x) +- maxError)

    spire.math.sin(jx).real should be(scala.math.sin(x))
    spire.math.sin(jx).infinitesimal(0) should be(scala.math.cos(x) +- maxError)

    spire.math.tan(jx).real should be(scala.math.tan(x))
    spire.math.tan(jx).infinitesimal(0) should be(
      1.0 + scala.math.tan(x) * scala.math.tan(x) +- maxError)
  }
  test("Evaluation and differentiation of inverse trigonometric functions") {
    val x = 0.133645
    val jx = x + Jet.h[Double](0)
    spire.math.acos(jx).real should be(scala.math.acos(x))
    spire.math.acos(jx).infinitesimal(0) should be(-1.0 / scala.math.sqrt(1.0 - x * x) +- maxError)

    spire.math.asin(jx).real should be(scala.math.asin(x))
    spire.math.asin(jx).infinitesimal(0) should be(1.0 / scala.math.sqrt(1.0 - x * x) +- maxError)

    spire.math.atan(jx).real should be(scala.math.atan(x))
    spire.math.atan(jx).infinitesimal(0) should be(1.0 / (1.0 + x * x) +- maxError)

    val y = 0.857264
    val jy = y + Jet.h[Double](1)
    val axy2 = spire.math.atan2(jy, jx)
    spire.math.atan2(jy, jx).real should be(scala.math.atan2(y, x))
    axy2.infinitesimal(0) should be(-y / (x * x + y * y) +- maxError)
    axy2.infinitesimal(1) should be(x / (x * x + y * y) +- maxError)
  }
  test("Evaluation and differentiation of hyperbolic functions") {
    val x = 0.9472836
    val jx = x + Jet.h[Double](0)
    spire.math.cosh(jx).real should be(scala.math.cosh(x))
    spire.math.cosh(jx).infinitesimal(0) should be(scala.math.sinh(x) +- maxError)

    spire.math.sinh(jx).real should be(scala.math.sinh(x))
    spire.math.sinh(jx).infinitesimal(0) should be(scala.math.cosh(x) +- maxError)

    spire.math.tanh(jx).real should be(scala.math.tanh(x))
    spire.math.tanh(jx).infinitesimal(0) should be(
      1.0 - scala.math.tanh(x) * scala.math.tanh(x) +- maxError)
  }
  test("Chain-rule differentiation") {
    def a[@sp(Double) T : Field : Trig](x: T): T = 2.0 * x * x - 3.14 * x + 2.71
    def b[@sp(Double) T : Field : Trig](x: T): T = 3.14 * x * x - spire.math.tan(x)
    def c[@sp(Double) T : Field : Trig](x: T): T = spire.math.acos(x) * spire.math.sin(x) + x

    def abc[@sp(Double) T : Field : Trig](x: T): T = a(b(c(x)))

    def da(x: Double) = 4.0 * x - 3.14
    def db(x: Double) = 3.14 * 2.0 * x - (1.0 + scala.math.tan(x) * scala.math.tan(x))
    def dc(x: Double) = 1.0 + (-1.0 / scala.math.sqrt(1.0 - x * x)) * scala.math.sin(x) +
      scala.math.acos(x) * scala.math.cos(x)
    def dabc(x: Double) = da(b(c(x))) * db(c(x)) * dc(x)

    val x = 0.293745
    val jx = x + Jet.h[Double](0)
    a(jx).infinitesimal(0) should be(da(x) +- maxError)
    b(jx).infinitesimal(0) should be(db(x) +- maxError)
    c(jx).infinitesimal(0) should be(dc(x) +- maxError)
    abc(jx).infinitesimal(0) should be(dabc(x) +- maxError)
    abc(jx).real should be(abc(x))
  }
  test("Evaluation and differentiation of a generic function") {
    def func[@sp(Double) T : Field : Trig](x: T): T = 3.14 * x * x - spire.math.tan(x)
    def dfunc(x: Double) = 3.14 * 2.0 * x - (1.0 + scala.math.tan(x) * scala.math.tan(x))
    val x = 0.293745
    val jx = x + Jet.h[Double](0)
    val jfunc = func(jx)
    jfunc.real should be(func(x))
    jfunc.infinitesimal(0) should be(dfunc(x) +- maxError)
  }
  test("Evaluation and differentiation of a generic function of two variables") {
    def func[@sp(Double) T : Field : Trig](x: T, y: T): T = 3.14 * x * y - spire.math.tan(x - y)
    def dfuncX(x: Double, y: Double) =
      3.14 * y - (1.0 + scala.math.tan(x - y) * scala.math.tan(x - y))
    def dfuncY(x: Double, y: Double) =
      3.14 * x + (1.0 + scala.math.tan(x - y) * scala.math.tan(x - y))
    val x = 0.293745
    val y = 1.2983764
    val jx = x + Jet.h[Double](0)
    val jy = y + Jet.h[Double](1)
    val jfunc = func(jx, jy)
    jfunc.real should be(func(x, y))
    jfunc.infinitesimal(0) should be(dfuncX(x, y) +- maxError)
    jfunc.infinitesimal(1) should be(dfuncY(x, y) +- maxError)
  }
}
