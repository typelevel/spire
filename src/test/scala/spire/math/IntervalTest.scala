package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}

class IntervalTest extends FunSuite {
  def cc(n1:Double, n2:Double) = ContinuousInterval(ClosedBelow(n1), ClosedAbove(n2))

  val a = cc(0.0, 4.0)
  val b = cc(-8.0, 2.0)
  val c = ContinuousInterval(OpenBelow(0.0), ClosedAbove(1.0))

  test("a.contains(0.0)") { assert(a.contains(0.0)) }
  test("a.crosses(0.0)") { assert(!a.crosses(0.0)) }
  test("a.contains(3.334)") { assert(a.contains(3.334)) }
  test("a.contains(8.334)") { assert(!a.contains(8.334)) }
  // test("a.abs") { assert(a.abs === a) }
  test("a + a") { assert(a + a === cc(0.0, 8.0)) }
  test("a - a") { assert(a - a === cc(-4.0, 4.0)) }
  test("a * a") { assert(a * a === cc(0.0, 16.0)) }

  test("b.contains(0.0)") { assert(b.contains(0.0)) }
  test("b.crosses(0.0)") { assert(b.crosses(0.0)) }
  // test("b.abs") { assert(b.abs === cc(0.0, 8.0)) }
  test("b + b") { assert(b + b === cc(-16.0, 4.0)) }
  test("b - b") { assert(b - b === cc(-10.0, 10.0)) }
  test("b * b") { assert(b * b === cc(-16.0, 64.0)) }

  test("c.contains(0.0)") { assert(!c.contains(0.0)) }
  test("c.crosses(0.0)") { assert(!c.crosses(0.0)) }
}

// TODO: this is just the tip of the iceberg... we also need to worry about
// unbounded intervals, closed vs open bounds, etc.
class ContinuousIntervalTest extends FunSuite {
  def cc(n1:Double, n2:Double) = ContinuousInterval(ClosedBelow(n1), ClosedAbove(n2))

  val a = 2.0
  val b = 5.0
  val c = 1.0
  val d = 4.0

  // numerator interval crosses zero
  test("[-a,b] / [c,d]") { assert(cc(-a, b) / cc(c, d) === cc(-a / c, b / c)) }
  test("[-a,b] / [-d,-c]") { assert(cc(-a, b) / cc(-d, -c) === cc(b / -c, -a / -c)) }

  // numerator interval is positive
  test("[a,b] / [-d,-c]") { assert(cc(a, b) / cc(-d, -c) === cc(b / -c, a / -d)) }
  test("[a,b] / [c,d]") { assert(cc(a, b) / cc(c, d) === cc(a / d, b / c)) }
  
  // numerator interval is negative
  test("[-b,-a] / [-d,-c]") { assert(cc(-b, -a) / cc(-d, -c) === cc(-a / -d, -b / -c)) }
  test("[-b,-a] / [c,d]") { assert(cc(-b, -a) / cc(c, d) === cc(-b / c, -a / d)) }
}
