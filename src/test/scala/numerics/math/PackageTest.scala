package numerics.math

import numerics.math.fun._

import org.scalatest.FunSuite

class PackageTest extends FunSuite {
  test("pow(Int, Int)") {
    assert(pow(2, 2) === 4)
  }

  test("pow(Long, Long)") {
    assert(pow(1000000000L, 2L) === 1000000000000000000L)
    assert(pow(1024L, 4L) === 1099511627776L)
    assert(pow(2L, 40L) === 1099511627776L)
    assert(pow(7L, 19L) === 11398895185373143L)
    assert(pow(13L, 17L) === 8650415919381337933L)
    assert(pow(13L, 17L) != pow(13.0, 17.0).toLong)
  }
}
