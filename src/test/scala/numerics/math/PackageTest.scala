package numerics.math

import numerics.math.fun._

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
}
