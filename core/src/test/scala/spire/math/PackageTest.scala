package spire.math

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

    assert(pow(BigDecimal("1.00000001"), BigDecimal("2000000000")) === BigDecimal("485165087.921736"))
    assert(pow(BigDecimal(Double.MaxValue) * 100, BigDecimal("0.00001")) === BigDecimal("1.0071694572064958"))
  }
}
