package spire
package math

import org.scalatest.FunSuite

class PackageTest extends FunSuite {
  test("pow(Int, Int)") {
    assert(pow(2, 2) === 4)
  }

  test("pow(Long, Long)") {
    assert(pow(1000000000L, 2) === 1000000000000000000L)
    assert(pow(1024L, 4) === 1099511627776L)
    assert(pow(2L, 40) === 1099511627776L)
    assert(pow(7L, 19) === 11398895185373143L)
    assert(pow(13L, 17) === 8650415919381337933L)
    assert(pow(13L, 17) != pow(13.0, 17.0).toLong)
  }

  test("pow(BigDecimal, BigDecimal)") {
    // NOTE: since the are rough approximations right now, changes to the
    // algorithm might produce more precise results but break these tests.

    // these are here more as a sanity check than as a commitment to these
    // exact values.
    assert(pow(BigDecimal(2), BigDecimal(5)) === BigDecimal(32))

    // wolfram alpha says:                                                         485165146.8932734862594040580748685
    assert(pow(BigDecimal("1.00000001"), BigDecimal("2000000000")) === BigDecimal("485165146.8932734862594042197965646"))

    // wolfram alpha says:                                                               1.007169457206495657176120750097341
    assert(pow(BigDecimal(Double.MaxValue) * 100, BigDecimal("0.00001")) === BigDecimal("1.007169457206495657262139152506902"))
  }

  test("hypot") {
    import spire.implicits._
    assert(hypot(3.0, 4.0) == 5.0)
    // check against overflow
    assert(hypot((3e20).toFloat, (4e20).toFloat) == 5e20.toFloat)
    // check the other branch of the if/else
    assert(hypot(4.0, 3.0) == 5.0)
    // and similarly against overflow
    assert(hypot((4e20).toFloat, (3e20).toFloat) == 5e20.toFloat)
    // check against division by zero
    assert(hypot(4.0, 0.0) == 4.0)
    assert(hypot(0.0, 4.0) == 4.0)
    assert(hypot(0.0, 0.0) == 0.0)
  }
}
