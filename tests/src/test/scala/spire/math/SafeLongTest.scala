package spire
package math

import org.scalatest.FunSuite
import spire.algebra._
import spire.util.Opt

class SafeLongTest extends FunSuite {
  test("===") {
    assert(SafeLong.one === SafeLong.one)
    assert(!(SafeLong.one =!= SafeLong.one))
  }
  test("getLong") {
    assert(SafeLong.one.getLong == Opt(1L))
    assert(SafeLong.safe64.getLong == Opt.empty[Long])
  }
  test("underlying") {
    assert(SafeLong.one.underlying.equals(1L))
    assert(SafeLong.safe64.underlying.equals(BigInt(SafeLong.big64)))
  }
  test("longValue") {
    assert(SafeLong.one.longValue == 1L)
    assert(SafeLong.safe64.longValue == Long.MinValue)
  }
  test("intValue") {
    assert(SafeLong.safe64.intValue == 0)
    assert(SafeLong.one.intValue == 1)
  }
  test("toBigDecimal") {
    assert(SafeLong.safe64.toBigDecimal == BigDecimal(SafeLong.big64))
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
    assert((SafeLong.one | SafeLong.one.toBigInt) == SafeLong.one)
  }
  test("^") {
    assert((SafeLong.one ^ SafeLong.one.toBigInt) == SafeLong.zero)
  }
  test("<<") {
    assert((SafeLong.minusOne << 1) == SafeLong(-1 << 1))
  }
  test("safeLongIsSigned") {
    for(x ← Seq(SafeLong.one)) {
      assert(Signed[SafeLong].signum(x) == x.signum)
      assert(Signed[SafeLong].abs(x) == x.abs)
      assert(IsIntegral[SafeLong].toDouble(x) == x.toDouble)
      assert(IsIntegral[SafeLong].toBigInt(x) == x.toBigInt)
    }
    for(a ← Seq(SafeLong.one, SafeLong.two); b ← Seq(SafeLong.one, SafeLong.two)) {
      assert(Order[SafeLong].compare(a, b) == a.compare(b))
      assert(EuclideanRing[SafeLong].equot(a, b) == a / b)
      assert(EuclideanRing[SafeLong].emod(a, b) == (a emod b))
      assert(EuclideanRing[SafeLong].equotmod(a, b) == (a equotmod b))
      assert(EuclideanRing[SafeLong].gcd(a, b) == a.gcd(b))
    }
    assert(Ring[SafeLong].fromInt(1) == SafeLong.one)
  }
  test("longGCD special cases") {
    assert(SafeLong.longGcd(Long.MinValue, 1L) == 1L)
  }
  test("pow") {
    assert(NRoot[SafeLong].fpow(SafeLong.one, SafeLong.one) == SafeLong.one)
    assert(NRoot[SafeLong].fpow(SafeLong.one, SafeLong.safe64) == SafeLong.one)
  }
}
