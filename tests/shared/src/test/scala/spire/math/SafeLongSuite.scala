package spire
package math

import spire.algebra._
import spire.util.Opt

class SafeLongSuite extends munit.FunSuite {
  test("===") {
    assertEquals(SafeLong.one, SafeLong.one)
    assert(!(SafeLong.one =!= SafeLong.one))
  }
  test("getLong") {
    assertEquals(SafeLong.one.getLong, Opt(1L))
    assertEquals(SafeLong.safe64.getLong, Opt.empty[Long])
  }
  test("underlying") {
    assert(SafeLong.one.underlying.equals(1L))
    assert(SafeLong.safe64.underlying.equals(BigInt(SafeLong.big64)))
  }
  test("longValue") {
    assertEquals(SafeLong.one.longValue, 1L)
    assertEquals(SafeLong.safe64.longValue, Long.MinValue)
  }
  test("intValue") {
    assertEquals(SafeLong.safe64.intValue, 0)
    assertEquals(SafeLong.one.intValue, 1)
  }
  test("toBigDecimal") {
    assertEquals(SafeLong.safe64.toBigDecimal, BigDecimal(SafeLong.big64))
  }
  test("isValidX") {
    assert(!SafeLong.safe64.isValidByte)
    assert(!SafeLong.safe64.isValidShort)
    assert(!SafeLong.safe64.isValidInt)
    assert(!SafeLong.safe64.isValidLong)
    assert(!SafeLong.safe64.isValidChar)
    assert(SafeLong.one.isValidByte)
    assert(SafeLong.one.isValidShort)
    assert(SafeLong.one.isValidInt)
    assert(SafeLong.one.isValidLong)
    assert(SafeLong.one.isValidChar)
  }
  test("|") {
    assertEquals((SafeLong.one | SafeLong.one.toBigInt), SafeLong.one)
  }
  test("^") {
    assertEquals((SafeLong.one ^ SafeLong.one.toBigInt), SafeLong.zero)
  }
  test("<<") {
    assertEquals((SafeLong.minusOne << 1), SafeLong(-1 << 1))
  }
  test("safeLongIsSigned") {
    for (x <- Seq(SafeLong.one)) {
      assertEquals(Signed[SafeLong].signum(x), x.signum)
      assertEquals(Signed[SafeLong].abs(x), x.abs)
      assertEquals(IsIntegral[SafeLong].toDouble(x), x.toDouble)
      assertEquals(IsIntegral[SafeLong].toBigInt(x), x.toBigInt)
    }
    for (a <- Seq(SafeLong.one, SafeLong.two); b <- Seq(SafeLong.one, SafeLong.two)) {
      assertEquals(Order[SafeLong].compare(a, b), a.compare(b))
      assertEquals(EuclideanRing[SafeLong].equot(a, b), a / b)
      assertEquals(EuclideanRing[SafeLong].emod(a, b), a % b)
      assertEquals(EuclideanRing[SafeLong].equotmod(a, b), a /% b)
      assertEquals(GCDRing[SafeLong].gcd(a, b), a.gcd(b))
    }
    assertEquals(Ring[SafeLong].fromInt(1), SafeLong.one)
  }
  test("longGCD special cases") {
    assert(SafeLong.longGcd(Long.MinValue, 1L) === 1L)
  }
  test("pow") {
    assertEquals(NRoot[SafeLong].fpow(SafeLong.one, SafeLong.one), SafeLong.one)
    assertEquals(NRoot[SafeLong].fpow(SafeLong.one, SafeLong.safe64), SafeLong.one)
  }
}
