package spire.math

import org.scalatest.FunSuite

class NumberTest extends FunSuite {
  test("create numbers") {
    Number(3.toByte)
    Number(3.toShort)
    Number(3)
    Number(3L)
    Number(3.0F)
    Number(3.0)
    Number("333333333333333333333333333333")
  }

  test("operations") {
    assert(Number(3) + Number(4) === Number(7))
    assert(Number(4) ** Number(30.0) === Number(1152921504606846976.0))

    assert(Number(100) ** Number(200.0) === Number(100) ** Number(200))
    assert(Number(100) ** Number(200) === Number("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))

    // sigh :(
    assert((Number("81") ** Number("0.5") - Number("9")).abs < 0.00000000000001)

    // tests near Long.MaxValue are nice because the floating point coverage is
    // sparser and fewer of the Long values can be exactly represented.
    val m = Long.MaxValue
    val d = m.toDouble
    
    // we do want to be able to see if Long values are exactly equal to
    // floating point values. this will only be possible when the Long value
    // can be exactly represented as a floating-point value (e.g. when
    // converting from a Double into a Long).
    assert(Number(d.toLong) === Number(d))
    
    // we don't want to coerce Long into Double when doing this test. Long
    // values will remain exact unless explicitly combined with inext
    // floating-point values.
    val n1 = Number(m - 1L)
    val n2 = Number(m.toDouble - 1.0)
    
    assert(n1 != n2)
  }
}
