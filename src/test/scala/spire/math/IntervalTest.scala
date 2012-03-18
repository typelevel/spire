package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}

class IntervalTest extends FunSuite {
  def closed(n1:Double, n2:Double) = RingInterval(ClosedBelow(n1), ClosedAbove(n2))

  val a = closed(0.0, 4.0)
  val b = closed(-8.0, 2.0)
  val c = RingInterval(OpenBelow(0.0), ClosedAbove(1.0))

  test("a.contains(0.0)") { assert(a.contains(0.0)) }
  test("a.crosses(0.0)") { assert(!a.crosses(0.0)) }
  test("a.contains(3.334)") { assert(a.contains(3.334)) }
  test("a.contains(8.334)") { assert(!a.contains(8.334)) }
  test("a.abs") { assert(a.abs === a) }
  test("a + a") { assert(a + a === closed(0.0, 8.0)) }
  test("a - a") { assert(a - a === closed(-4.0, 4.0)) }
  test("a * a") { assert(a * a === closed(0.0, 16.0)) }

  test("b.contains(0.0)") { assert(b.contains(0.0)) }
  test("b.crosses(0.0)") { assert(b.crosses(0.0)) }
  test("b.abs") { assert(b.abs === closed(0.0, 8.0)) }
  test("b + b") { assert(b + b === closed(-16.0, 4.0)) }
  test("b - b") { assert(b - b === closed(-10.0, 10.0)) }
  test("b * b") { assert(b * b === closed(-16.0, 64.0)) }

  test("c.contains(0.0)") { assert(!c.contains(0.0)) }
  test("c.crosses(0.0)") { assert(!c.crosses(0.0)) }
}
