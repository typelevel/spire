package spire
package math

import spire.implicits._

class RingIntervalSuite extends munit.FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)

  val a = cc(0.0, 4.0)
  test("a + a") { assertEquals(a + a, cc(0.0, 8.0)) }
  test("a - a") { assertEquals(a - a, cc(-4.0, 4.0)) }
  test("a * a") { assertEquals(a * a, cc(0.0, 16.0)) }

  val b = cc(-8.0, 2.0)
  test("b + b") { assertEquals(b + b, cc(-16.0, 4.0)) }
  test("b - b") { assertEquals(b - b, cc(-10.0, 10.0)) }
  test("b * b") { assertEquals(b * b, cc(-16.0, 64.0)) }

  import interval.{Closed, Open, Unbound}
  val c = 4.0
  test("-(c, ∞) =  (-∞, -c)") {
    assertEquals(-Interval.fromBounds(Open(c), Unbound()), Interval.fromBounds(Unbound(), Open(-c)))
  }
  test("-(-∞, c] =  [-c, ∞)") {
    assertEquals(-Interval.fromBounds(Unbound(), Closed(c)), Interval.fromBounds(Closed(-c), Unbound()))
  }
  test("(c, ∞) * (-c) =  (-∞, -c * c), c > 0") {
    assertEquals(Interval.fromBounds(Open(c), Unbound()) * (-c), Interval.fromBounds(Unbound(), Open(-c * c)))
  }
  test("(-∞, c] * (-c) =  [-c * c, ∞), c > 0") {
    assertEquals(Interval.fromBounds(Unbound(), Closed(c)) * (-c), Interval.fromBounds(Closed(-c * c), Unbound()))
  }
  test("Interval multiplication bug #372") {
    val a = Interval(-1, 1)
    val b = Interval.above(1)
    val x = -1
    val y = 10
    assert(a.contains(x))
    assert(b.contains(y))
    assert((a * b).contains(x * y))
  }
  test("Interval multiplication bug 1") {
    val a = Interval(-3, -2)
    val b = Interval.above(-10)
    val x = -3
    val y = -9
    assert(a.contains(x))
    assert(b.contains(y))
    assert((a * b).contains(x * y))
  }
  test("Interval multiplication bug 2") {
    val a = Interval.atOrBelow(0)
    val b = Interval.below(-1)
    assert((a * b).contains(0))
  }
  test("Interval multiplication bug 3") {
    val a = Interval.atOrBelow(0)
    val b = Interval.open(-2, -1)
    assert((a * b).contains(0))
  }
  test("Interval multiplication bug 4") {
    val a = Interval.above(2)
    val b = Interval.closed(0, 1)
    assert((a * b).contains(0))
  }
}
