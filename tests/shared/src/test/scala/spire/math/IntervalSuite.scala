package spire
package math

import spire.implicits.{eqOps => _, _}

class IntervalSuite extends munit.FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)
  def co(n1: Double, n2: Double) = Interval.openUpper(n1, n2)
  def oc(n1: Double, n2: Double) = Interval.openLower(n1, n2)
  def oo(n1: Double, n2: Double) = Interval.open(n1, n2)

  val e = Interval.empty[Double]
  val all = Interval.all[Double]

  test("[2, inf] is a superset of empty") { assert(Interval.atOrAbove(2).isSupersetOf(Interval.empty[Int])) }
  test("empty is empty") { assert(e.isEmpty) }
  test("point is point") { assert(Interval.point(2).isPoint) }
  test("[2,2] is point") { assert(Interval.closed(2, 2).isPoint) }
  test("[3,2] is empty") { assert(Interval.closed(3, 2).isEmpty) }
  test("empty interval is not above -1") { assert(!Interval.empty[Int].hasAbove(-1)) }
  test("empty interval is not below 1") { assert(!Interval.empty[Int].hasBelow(1)) }
  test("[2] has above 0") { assert(Interval.point(2).hasAbove(0)) }
  test("[-2] has below 0") { assert(Interval.point(-2).hasBelow(0)) }
  test("[0, 1] has at or above 1") { assert(Interval.closed(0, 1).hasAtOrAbove(1)) }
  test("[1, 2] has at or above 1") { assert(Interval.closed(1, 2).hasAtOrAbove(1)) }
  test("[1, 2] has above 1") { assert(Interval.closed(1, 2).hasAtOrAbove(1)) }
  test("(1, 2] has above 1") { assert(Interval.openLower(1, 2).hasAtOrAbove(1)) }

  test("Interval.above(0).isBounded is false") { assert(!Interval.above(0).isBounded) }
  test("Interval.closed(0, 5).isBounded is true") { assert(Interval.closed(0, 5).isBounded) }

  test("Interval.point(2).toString == [2]") { assertEquals(Interval.point(2).toString, "[2]") }
  test("Interval.empty.toString == (Ø)") { assertEquals(Interval.empty[Int].toString, "(Ø)") }

  val a = cc(0.0, 4.0)
  test("a.contains(0.0) is true") { assertEquals(a.contains(0.0), true) }
  test("a.crosses(0.0) is false") { assertEquals(a.crosses(0.0), false) }
  test("a.contains(3.334) is true") { assertEquals(a.contains(3.334), true) }
  test("a.contains(8.334) is false") { assertEquals(a.contains(8.334), false) }

  val b = cc(-8.0, 2.0)
  test("b.contains(0.0) is true") { assertEquals(b.contains(0.0), true) }
  test("b.crosses(0.0) is true") { assertEquals(b.crosses(0.0), true) }

  val c = oc(0.0, 1.0)
  test("c.contains(0.0) is false") { assertEquals(c.contains(0.0), false) }
  test("c.crosses(0.0) is false") { assertEquals(c.crosses(0.0), false) }

  test("[3, 6] -- [3, 6] = nil") { assertEquals(cc(3D, 6D) -- cc(3D, 6D), Nil) }
  test("[3, 6] -- empty = [3, 6]") { assertEquals(cc(3D, 6D) -- e, List(cc(3D, 6D))) }
  test("[3, 6] -- all = nil") { assertEquals(cc(3D, 6D) -- all, Nil) }
  test("[3, 6] -- [4, 6] = [3, 4)") { assertEquals(cc(3D, 6D) -- cc(4D, 6D), List(co(3D, 4D))) }
  test("[3, 6] -- [4, 5] = [3, 4), (5, 6]") { assertEquals(cc(3D, 6D) -- cc(4D, 5D), List(co(3D, 4D), oc(5D, 6D))) }
}

