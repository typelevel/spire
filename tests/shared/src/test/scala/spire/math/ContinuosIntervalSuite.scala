package spire
package math

import spire.implicits.{eqOps => _, _}

// TODO: this is just the tip of the iceberg... we also need to worry about
// unbounded intervals, closed vs open bounds, etc.
class ContinuousIntervalSuite extends munit.FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)

  val a = 2.0
  val b = 5.0
  val c = 1.0
  val d = 4.0

  // numerator interval crosses zero
  test("[-a,b] / [c,d]") { assertEquals(cc(-a, b) / cc(c, d), cc(-a / c, b / c)) }
  test("[-a,b] / [-d,-c]") { assertEquals(cc(-a, b) / cc(-d, -c), cc(b / -c, -a / -c)) }

  // numerator interval is positive
  test("[a,b] / [-d,-c]") { assertEquals(cc(a, b) / cc(-d, -c), cc(b / -c, a / -d)) }
  test("[a,b] / [c,d]") { assertEquals(cc(a, b) / cc(c, d), cc(a / d, b / c)) }

  // numerator interval is negative
  test("[-b,-a] / [-d,-c]") { assertEquals(cc(-b, -a) / cc(-d, -c), cc(-a / -d, -b / -c)) }
  test("[-b,-a] / [c,d]") { assertEquals(cc(-b, -a) / cc(c, d), cc(-b / c, -a / d)) }
}
