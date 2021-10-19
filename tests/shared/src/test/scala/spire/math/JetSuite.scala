package spire
package math

import spire.algebra._
import spire.implicits._
import java.util.Arrays

class JetSuite extends munit.FunSuite {

  // Default test with 3-dimensional Jet's
  implicit val dim: JetDim = JetDim(3)
  val maxError = 1.0e-12

  test("JetDim") {
    assertEquals(dim.dimension, 3)
  }

  test("Jet(scalar, array) constructor") {
    val a = Array[Double](2.3, 3.4, 4.5)
    val j = new Jet[Double](8.9, a)
    assertEquals(j.dimension, dim.dimension)
    assertEquals(j.jetDimension.dimension, dim.dimension)
    assertEquals(j.infinitesimal.size, dim.dimension)
    assertEquals(j.real, 8.9)
    assert(Arrays.equals(j.infinitesimal.toArray, a.toArray))
  }
  test("Jet constructor yields a zero jet") {
    val jz = Jet[Double]
    assertEquals(jz.real, 0.0)
    assert(jz.isReal)
    assertEquals(jz.infinitesimal.size, dim.dimension)
    assertEquals(jz.isInfinitesimal, false)
    assert(jz.isZero)
  }
  test("Jet.zero yields a zero jet") {
    val jzz = Jet.zero[Double]
    assertEquals(jzz.real, 0.0)
    assert(jzz.isReal)
    assertEquals(jzz.infinitesimal.size, dim.dimension)
    assertEquals(jzz.isInfinitesimal, false)
    assert(jzz.isZero)
  }
  test("Jet.one yields a unitary jet") {
    val jo = Jet.one[Double]
    assertEquals(jo.real, 1.0)
    assert(jo.isReal)
    assertEquals(jo.infinitesimal.size, dim.dimension)
    assertEquals(jo.isInfinitesimal, false)
  }
  test("Jet.h yields an infinitesimal jet") {
    val jk = Jet.h[Double](k = 1)
    assertEquals(jk.real, 0.0)
    assertEquals(jk.isReal, false)
    assert(jk.isInfinitesimal)
    assert(Arrays.equals(jk.infinitesimal.toArray, Array(0.0, 1.0, 0.0)))
  }
  test("Jet(x, k) yields a jet for evaluating a function and its k-th partial derivative") {
    val jk = Jet(2.3, k = 2)
    assertEquals(jk.real, 2.3)
    assertEquals(jk.isReal, false)
    assertEquals(jk.isInfinitesimal, false)
    assert(Arrays.equals(jk.infinitesimal.toArray, Array(0.0, 0.0, 1.0)))
  }
  test("Jet(x) constructors from scalars") {
    val jf = Jet(2.0f)
    assertEquals(jf.real, 2.0f)
    assert(jf.isReal)
    assertEquals(jf.infinitesimal.size, dim.dimension)
    assertEquals(jf.isInfinitesimal, false)

    val jd = Jet(2.6)
    assertEquals(jd.real, 2.6)
    assert(jd.isReal)
    assertEquals(jd.infinitesimal.size, dim.dimension)
    assertEquals(jd.isInfinitesimal, false)

    val jbd = Jet(BigDecimal(2847.694984))
    assertEquals(jbd.real, BigDecimal(2847.694984))
    assert(jbd.isReal)
    assertEquals(jbd.infinitesimal.size, dim.dimension)
    assertEquals(jbd.isInfinitesimal, false)

    val jfi = Jet.fromInt[Float](2)
    assertEquals(jfi.real, 2.0f)
    assert(jfi.isReal)
    assertEquals(jfi.infinitesimal.size, dim.dimension)
    assertEquals(jfi.isInfinitesimal, false)
  }
  test("Conversions from scalars") {
    val jfi = Jet.intToJet(2)
    assertEquals(jfi.real, 2.0)
    assert(jfi.isReal)
    assertEquals(jfi.infinitesimal.size, dim.dimension)
    assertEquals(jfi.isInfinitesimal, false)

    val jfl = Jet.longToJet(2L)
    assertEquals(jfl.real, 2.0)
    assert(jfl.isReal)
    assertEquals(jfl.infinitesimal.size, dim.dimension)
    assertEquals(jfl.isInfinitesimal, false)

    val jff = Jet.floatToJet(2.47f)
    assertEquals(jff.real, 2.47f)
    assert(jff.isReal)
    assertEquals(jff.infinitesimal.size, dim.dimension)
    assertEquals(jff.isInfinitesimal, false)

    val jfd = Jet.doubleToJet(2.47)
    assertEquals(jfd.real, 2.47)
    assert(jfd.isReal)
    assertEquals(jfd.infinitesimal.size, dim.dimension)
    assertEquals(jfd.isInfinitesimal, false)

    val jfbi = Jet.bigIntToJet(BigInt(247847))
    assertEquals(jfbi.real, BigDecimal(247847))
    assert(jfbi.isReal)
    assertEquals(jfbi.infinitesimal.size, dim.dimension)
    assertEquals(jfbi.isInfinitesimal, false)

    val jfbd = Jet.bigDecimalToJet(BigDecimal(247847.28375))
    assertEquals(jfbd.real, BigDecimal(247847.28375))
    assert(jfbd.isReal)
    assertEquals(jfbd.infinitesimal.size, dim.dimension)
    assertEquals(jfbd.isInfinitesimal, false)
  }
  test("Conversions to scalars") {
    val j = Jet(8.7, Array(7.97, 9.31, 0.0))
    assertEquals(j.doubleValue, 8.7)
    assertEquals(j.floatValue, 8.7f)
    assertEquals(j.longValue, 8L)
    assertEquals(j.intValue, 8)
    assertEquals(j.shortValue, 8.toShort)
    assertEquals(j.byteValue, 8.toByte)
  }
  test("Conversion to tuple") {
    val j = Jet(-3.1, Array(1.0, 2.1, 3.3))
    val tj = j.asTuple
    assertEquals(tj._1, j.real)
    assertEquals(tj._2, j.infinitesimal)
  }
  test("Signed") {
    assertEquals(Jet(9.1).signum, 1)
    assertEquals(Jet(-3.1, Array(1.0, 2.1, 3.3)).signum, -1)
  }
  test("isWhole iff real and integer") {
    assert(Jet(4.0f).isWhole)
    assertEquals(Jet(9.2).isWhole, false)
    assertEquals(Jet(3.0f, Array(1.0f, 2.0f, 3.0f)).isWhole, false)
  }
  test("isValidInt iff real, whole and in range") {
    assert(Jet(-4.0).isValidInt)
    assertEquals(Jet(4.1).isValidInt, false)
    assertEquals(Jet(3.0f, Array(1.0f, 2.0f, 3.0f)).isValidInt, false)
    assertEquals(Jet(Int.MinValue.toDouble - 1000.0).isValidInt, false)
    assertEquals(Jet(Int.MaxValue.toDouble + 1000.0).isValidInt, false)
  }
  test("Equality-comparable and hashable") {
    val r = 13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    val b = Jet(r.toDouble, i.map(_.toDouble))
    val c = Jet(b.real.toFloat, b.infinitesimal.map(_.toFloat))
    // Value-based, symmetric, reflexive, transitive
    assert(a === a)
    assert(a === b)
    assert(b === a)
    assert(b === c)
    assert(a === c)
    // Same for eqv
    val bf: Jet[Float] = a.copy()
    val cf: Jet[Float] = a.copy()
    assert(a.eqv(a))
    assert(a.eqv(bf))
    assert(bf.eqv(a))
    assert(bf.eqv(cf))
    assert(a.eqv(cf))
    // Reverse for neqv
    assertEquals(a.neqv(a), false)
    assertEquals(a.neqv(bf), false)
    assertEquals(bf.neqv(a), false)
    assertEquals(bf.neqv(cf), false)
    assertEquals(a.neqv(cf), false)
    // Likewise for hashes
    assertEquals(a.hashCode, a.hashCode)
    assertEquals(a.hashCode, b.hashCode)
    assertEquals(b.hashCode, a.hashCode)
    assertEquals(b.hashCode, c.hashCode)
    assertEquals(a.hashCode, c.hashCode)
  }
  test("Prints into a human-readable string") {
    val compVM = Jet(2.4, k = 2).toString == "(2.4 + [0.0, 0.0, 1.0]h)"
    val compJS = Jet(2.4, k = 2).toString == "(2.4 + [0, 0, 1]h)"
    assert(compVM || compJS)
  }
  test("Unary minus operator") {
    val r = -13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    assertEquals(-a, new Jet(-r, -i))
    assertEquals(-(-a), a)
  }
  test("Arithmetic combinations with scalars") {
    val r = 13.0f
    val i = Array(1.0f, 2.0f, 3.0f)
    val a = Jet(r, i)
    val b = 97.0f
    assertEquals((a + b), a.copy(real = r + b))
    assertEquals((a - b), a.copy(real = r - b))
    assertEquals((a * b), a.copy(real = r * b, infinitesimal = i :* b))
    assertEquals((a / b), a.copy(real = r / b, infinitesimal = i :/ b))
  }
  test("Arithmetic combinations with scalar and non-scalar Jets") {
    val a = Jet(1.0, Array(2.0, 3.0, 4.0))
    val b = Jet(2.0)
    assertEquals((a + b), Jet(a.real + b.real, a.infinitesimal))
    assertEquals((a - b), Jet(a.real - b.real, a.infinitesimal))
    assertEquals((a * b), Jet(a.real * b.real, a.infinitesimal :* b.real))
    assertEquals((a / b), Jet(a.real / b.real, a.infinitesimal :/ b.real))
  }
  test("Evaluation and differentiation of arithmetic operations") {
    def a[@sp(Double) T: Field: Trig](x: T): T = 2.0 * x + spire.math.cos[T](x)
    def b[@sp(Double) T: Field: Trig](x: T): T = spire.math.sin(x) - spire.math.log[T](x) + 7.3
    def da(x: Double) = 2.0 - scala.math.sin(x)
    def db(x: Double) = scala.math.cos(x) - 1 / x

    val x = 894.3287562
    val jx = x + Jet.h[Double](0)

    def a_plus_b[@sp(Double) T: Field: Trig](x: T): T = a(x) + b(x)
    assertEquals(a_plus_b(jx).real, a(x) + b(x))
    assertEqualsDouble(a_plus_b(jx).infinitesimal(0), da(x) + db(x), maxError)

    def a_minus_b[@sp(Double) T: Field: Trig](x: T): T = a(x) - b(x)
    assertEquals(a_minus_b(jx).real, a(x) - b(x))
    assertEqualsDouble(a_minus_b(jx).infinitesimal(0), da(x) - db(x), maxError)

    def a_times_b[@sp(Double) T: Field: Trig](x: T): T = a(x) * b(x)
    assertEquals(a_times_b(jx).real, a(x) * b(x))
    assertEqualsDouble(a_times_b(jx).infinitesimal(0), da(x) * b(x) + a(x) * db(x), maxError)

    def a_div_b[@sp(Double) T: Field: Trig](x: T): T = a(x) / b(x)
    assertEquals(a_div_b(jx).real, a(x) / b(x))
    assertEqualsDouble(a_div_b(jx).infinitesimal(0), (da(x) * b(x) - a(x) * db(x)) / (b(x) * b(x)), maxError)
  }
  test("Evaluation and differentiation of exponential and logarithm") {
    val x = 27.98847750
    val jx = x + Jet.h[Double](0)
    val lx = spire.math.log(jx)
    assertEquals(lx.real, scala.math.log(x))
    assertEqualsDouble(lx.infinitesimal(0), 1.0 / x, maxError)

    val ex = spire.math.exp(jx)
    assertEquals(ex.real, scala.math.exp(x))
    assertEqualsDouble(ex.infinitesimal(0), scala.math.exp(x), maxError)
  }
  test("Evaluation and differentiation of powers and roots") {
    // Constant integer exponent: x ** n
    val x = 9.3874983
    val jx = x + Jet.h[Double](0)
    val n = 5
    val jxn = jx.pow(n)
    assertEqualsDouble(jxn.real, scala.math.pow(x, n), maxError)
    assertEqualsDouble(jxn.infinitesimal(0), n * scala.math.pow(x, n - 1), maxError)
    // Constant double exponent: x ** d
    val d = 0.387
    val jxd = jx.pow(d)
    assertEquals(jxd.real, scala.math.pow(x, d))
    assertEqualsDouble(jxd.infinitesimal(0), scala.math.exp(d * scala.math.log(x)) * d / x, maxError)
    // Variable base and exponent: sin(x) ** x
    val jex = spire.math.sin(jx)
    val jp = jx.pow(jex)
    assertEqualsDouble(jp.real, scala.math.pow(x, scala.math.sin(x)), maxError)
    assertEqualsDouble(jp.infinitesimal(0),
                       scala.math.exp(scala.math.sin(x) * scala.math.log(x)) *
                         (scala.math.cos(x) * scala.math.log(x) + scala.math.sin(x) / x),
                       maxError
    )
    // Square root
    val sq = spire.math.sqrt(jx)
    assertEquals(sq.real, scala.math.sqrt(x))
    assertEquals(sq.infinitesimal(0), 0.5 / scala.math.sqrt(x))
  }
  test("Evaluation and differentiation of trigonometric functions") {
    val x = 0.8377469
    val jx = x + Jet.h[Double](0)
    assertEquals(spire.math.cos(jx).real, scala.math.cos(x))
    assertEqualsDouble(spire.math.cos(jx).infinitesimal(0), -scala.math.sin(x), maxError)

    assertEquals(spire.math.sin(jx).real, scala.math.sin(x))
    assertEqualsDouble(spire.math.sin(jx).infinitesimal(0), scala.math.cos(x), maxError)

    assertEquals(spire.math.tan(jx).real, scala.math.tan(x))
    assertEqualsDouble(spire.math.tan(jx).infinitesimal(0), 1.0 + scala.math.tan(x) * scala.math.tan(x), maxError)
  }
  test("Evaluation and differentiation of inverse trigonometric functions") {
    val x = 0.133645
    val jx = x + Jet.h[Double](0)
    assertEquals(spire.math.acos(jx).real, scala.math.acos(x))
    assertEqualsDouble(spire.math.acos(jx).infinitesimal(0), -1.0 / scala.math.sqrt(1.0 - x * x), maxError)

    assertEquals(spire.math.asin(jx).real, scala.math.asin(x))
    assertEqualsDouble(spire.math.asin(jx).infinitesimal(0), 1.0 / scala.math.sqrt(1.0 - x * x), maxError)

    assertEquals(spire.math.atan(jx).real, scala.math.atan(x))
    assertEqualsDouble(spire.math.atan(jx).infinitesimal(0), 1.0 / (1.0 + x * x), maxError)

    val y = 0.857264
    val jy = y + Jet.h[Double](1)
    val axy2 = spire.math.atan2(jy, jx)
    assertEquals(spire.math.atan2(jy, jx).real, scala.math.atan2(y, x))
    assertEqualsDouble(axy2.infinitesimal(0), -y / (x * x + y * y), maxError)
    assertEqualsDouble(axy2.infinitesimal(1), x / (x * x + y * y), maxError)
  }
  test("Evaluation and differentiation of hyperbolic functions") {
    val x = 0.9472836
    val jx = x + Jet.h[Double](0)
    assertEquals(spire.math.cosh(jx).real, scala.math.cosh(x))
    assertEqualsDouble(spire.math.cosh(jx).infinitesimal(0), scala.math.sinh(x), maxError)

    assertEquals(spire.math.sinh(jx).real, scala.math.sinh(x))
    assertEqualsDouble(spire.math.sinh(jx).infinitesimal(0), scala.math.cosh(x), maxError)

    assertEquals(spire.math.tanh(jx).real, scala.math.tanh(x))
    assertEqualsDouble(spire.math.tanh(jx).infinitesimal(0), 1.0 - scala.math.tanh(x) * scala.math.tanh(x), maxError)
  }
  test("Chain-rule differentiation") {
    def a[@sp(Double) T: Field: Trig](x: T): T = 2.0 * x * x - 3.14 * x + 2.71
    def b[@sp(Double) T: Field: Trig](x: T): T = 3.14 * x * x - spire.math.tan(x)
    def c[@sp(Double) T: Field: Trig](x: T): T = spire.math.acos(x) * spire.math.sin(x) + x

    def abc[@sp(Double) T: Field: Trig](x: T): T = a(b(c(x)))

    def da(x: Double) = 4.0 * x - 3.14
    def db(x: Double) = 3.14 * 2.0 * x - (1.0 + scala.math.tan(x) * scala.math.tan(x))
    def dc(x: Double) = 1.0 + (-1.0 / scala.math.sqrt(1.0 - x * x)) * scala.math.sin(x) +
      scala.math.acos(x) * scala.math.cos(x)
    def dabc(x: Double) = da(b(c(x))) * db(c(x)) * dc(x)

    val x = 0.293745
    val jx = x + Jet.h[Double](0)
    assertEqualsDouble(a(jx).infinitesimal(0), da(x), maxError)
    assertEqualsDouble(b(jx).infinitesimal(0), db(x), maxError)
    assertEqualsDouble(c(jx).infinitesimal(0), dc(x), maxError)
    assertEqualsDouble(abc(jx).infinitesimal(0), dabc(x), maxError)
    assertEquals(abc(jx).real, abc(x))
  }
  test("Evaluation and differentiation of a generic function") {
    def func[@sp(Double) T: Field: Trig](x: T): T = 3.14 * x * x - spire.math.tan(x)
    def dfunc(x: Double) = 3.14 * 2.0 * x - (1.0 + scala.math.tan(x) * scala.math.tan(x))
    val x = 0.293745
    val jx = x + Jet.h[Double](0)
    val jfunc = func(jx)
    assertEquals(jfunc.real, func(x))
    assertEqualsDouble(jfunc.infinitesimal(0), dfunc(x), maxError)
  }
  test("Evaluation and differentiation of a generic function of two variables") {
    def func[@sp(Double) T: Field: Trig](x: T, y: T): T = 3.14 * x * y - spire.math.tan(x - y)
    def dfuncX(x: Double, y: Double) =
      3.14 * y - (1.0 + scala.math.tan(x - y) * scala.math.tan(x - y))
    def dfuncY(x: Double, y: Double) =
      3.14 * x + (1.0 + scala.math.tan(x - y) * scala.math.tan(x - y))
    val x = 0.293745
    val y = 1.2983764
    val jx = x + Jet.h[Double](0)
    val jy = y + Jet.h[Double](1)
    val jfunc = func(jx, jy)
    assertEquals(jfunc.real, func(x, y))
    assertEqualsDouble(jfunc.infinitesimal(0), dfuncX(x, y), maxError)
    assertEqualsDouble(jfunc.infinitesimal(1), dfuncY(x, y), maxError)
  }
}
