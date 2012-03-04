package numerics.math.fpf

import numerics.math._
import org.scalatest.FunSuite


class MaybeDoubleTest extends FunSuite {
  test("Invalid is invalid") {
    assert(!MaybeDouble.Invalid.isValid)
  }

  test("Exact construction from Float, Double, and Int") {
    assert(MaybeDouble(0.3f).isExact)
    assert(MaybeDouble(33e100).isExact)
    assert(MaybeDouble(1234).isExact)
    assert(MaybeDouble(Int.MinValue).isExact)
  }

  test("In-bounds Long construction is exact") {
    
    // Technically, a Double can hold 1L << 53, but only because the last digit
    // is a 0. That is, a Double can also hold 1L << 62 exactly, because it only
    // has 1 digit in the mantissa. However, (1L << 53) - 1 is the largest
    // exact integer a Double can hold that whose bits are all 1's.

    assert(MaybeDouble((1L << 53) - 1).isExact)
  }

  test("Out-of-bounds Long construction is not exact") {
    assert(!MaybeDouble(1L << 53).isExact)
    assert(!MaybeDouble(Long.MaxValue).isExact)
  }

  test("In-bounds BigInt construction is exact") {
    assert(MaybeDouble((BigInt(1) << 53) - 1).isExact)
  }

  test("Out-of-bounds BigInt construction is not exact") {
    assert(!MaybeDouble(BigInt(1) << 53).isExact)
    assert(!MaybeDouble(-(BigInt(17389) * 17387 * 17383 * 17377 * 17359)).isExact)
  }

  test("Rational construction is approximate") {
    val md = MaybeDouble(Rational(1, 3))
    assert(!md.isExact)
    assert(md.error > 0.0)
  }

  test("BigDecimal construction is approximate") {
    
    // Truthfully, it would be nice if BigDecimal construction would be exact
    // when it can. What would be better is a base-2 BigFloat class.

    val md = MaybeDouble(BigDecimal(0.5))
    assert(!md.isExact)
    assert(md.error > 0.0)
  }

  def assertAlmostEqual(a: MaybeDouble, x: Double) {
    assert(a.approx - a.error <= x && x <= a.approx + a.error)
  }


  test("Ring ops keep correct error bound") {
    val a = BigDecimal("1.234568")
    val b = BigDecimal("9281234.199991111111111111111002304291")
    val c = BigDecimal("0.333333333333333333333333333333333333333333")
    val a_ = MaybeDouble(a)
    val b_ = MaybeDouble(b)
    val c_ = MaybeDouble(c)

    // I dunno, just do some ish
    val bdx = a * b * b * b + c * a - a
    val mdx = a_ * b_ * b_ * b_ + c_ * a_ - a_
    
    assertAlmostEqual(mdx, bdx.toDouble)

    assertAlmostEqual(a_ * b_, (a * b).toDouble)
    assertAlmostEqual(a_ + b_, (a + b).toDouble)
    assertAlmostEqual(a_ - b_, (a - b).toDouble)
  }

  test("Division keeps the faith") {
    val a = Rational(1)
    val b = Rational(3)
    val a_ = MaybeDouble(a)
    val b_ = MaybeDouble(b)

    assertAlmostEqual(a_ / b_, (a / b).toDouble)

    val x = Iterator.fill(23)(b_).foldLeft(a_)(_ / _)
    assertAlmostEqual(x, a / b pow 23 toDouble)
  }

  test("Square-roots keep correct error bound") {
    val a = Real(2)
    val a_ = MaybeDouble(2)

    assertAlmostEqual(MaybeDouble(2).sqrt, Real(2).sqrt.toDouble)
    assertAlmostEqual(MaybeDouble(4).sqrt, Real(4).sqrt.toDouble)
  }

  test("abs doesn't affect error") {
    val a_ = MaybeDouble(Rational(-1, 3))
    assert(a_.abs === -a_)
    assert(MaybeDouble(1).abs === MaybeDouble(1))
    assert(a_.abs.error === a_.error)
  }

  test("Unary-minus don't affect sign") {
    val a = MaybeDouble(1.0)
    assert(-a.error === a.error)
    assert(-(-a) === a)
    assert(-a === MaybeDouble(-1.0))
  }

  test("Sign returns None when not exact") {
    val a = MaybeDouble.approx(1)
    assert((a - a).sign === None)

    // Note: x.sqrt - y.sqrt - z.sqrt == 0.

    val x = MaybeDouble(18)
    val y = MaybeDouble(8)
    val z = MaybeDouble(2)
    assert((x.sqrt - (y.sqrt + z.sqrt)).sign === None)
  }

  test("Sign returns Some(sign) when it can") {
    val x = MaybeDouble(3.08)
    val r = MaybeDouble(19).sqrt
    assert((r - (x*x + x*x).sqrt).sign == Some(Positive))
  }

  test("Conversion functions can be exact") {
    val x = Iterator.fill(23)(MaybeDouble(3.0)).foldLeft(MaybeDouble(1.0))(_ / _)
    assert(x.toFloat.isDefined)
    assert(x.toInt === Some(0))
    assert(MaybeDouble(Int.MaxValue).toInt === Some(Int.MaxValue))
    assert(MaybeDouble(1.0).toDouble == Some(1.0))
  }

  test("Conversion functions return None when not exact") {
    assert(MaybeDouble(Float.MaxValue.toDouble * 2).toFloat == None)

    // Turns out it is pretty hard to get an error so bad it isn't even a Float.

    val a = Iterator.fill(10)(MaybeDouble.approx(10000.0)) reduce (_ * _)
    val b = Iterator.fill(10)(MaybeDouble.approx(1 / 10000.0)) reduce (_ * _)
    val x = Iterator.fill(10000)(a * b) reduce (_ + _)
    assert((x - x).toFloat === None)

    assert(MaybeDouble.approx(1.0).toLong === None)
    assert(MaybeDouble.approx(-1.0).toInt === None)
    assert(MaybeDouble.approx(4.0).toBigInt === None)

    assert(MaybeDouble.approx(1.0).toDouble === None)
  }
}




