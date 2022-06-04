/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.implicits._

class IntervalSyntaxSuite extends munit.FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)
  def co(n1: Double, n2: Double) = Interval.openUpper(n1, n2)
  def oc(n1: Double, n2: Double) = Interval.openLower(n1, n2)
  def oo(n1: Double, n2: Double) = Interval.open(n1, n2)

  val e = Interval.empty[Double]
  val all = Interval.all[Double]

  val a = cc(-1.0, 4)
  test("a ∋ 0.0 is true") { assertEquals(a ∋ 0.0, true) }
  test("a ∋ -1.0 is true") { assertEquals(a ∋ -1.0, true) }
  test("a ∋ 4.0 is true") { assertEquals(a ∋ 4.0, true) }
  test("a ∋ 5 is false") { assertEquals(a ∋ 5.0, false) }
  test("a ∋ -2 is false") { assertEquals(a ∋ -2.0, false) }

  test("a ∌ 0.0 is false") { assertEquals(a ∌ 0.0, false) }
  test("a ∌ -1.0 is false") { assertEquals(a ∌ -1.0, false) }
  test("a ∌ 4.0 is false") { assertEquals(a ∌ 4.0, false) }
  test("a ∌ 5 is true") { assertEquals(a ∌ 5.0, true) }
  test("a ∌ -2 is true") { assertEquals(a ∌ -2.0, true) }

  test("0.0 ∈: a is true") { assertEquals(0.0 ∈: a, true) }
  test("-1.0 ∈: a is true") { assertEquals(-1.0 ∈: a, true) }
  test("4.0 ∈: a is true") { assertEquals(4.0 ∈: a, true) }
  test("5 ∈: a is false") { assertEquals(5.0 ∈: a, false) }
  test("-2 ∈: a is false") { assertEquals(-2.0 ∈: a, false) }

  test("0.0 ∉: a is false") { assertEquals(0.0 ∉: a, false) }
  test("-1.0 ∉: a is false") { assertEquals(-1.0 ∉: a, false) }
  test("4.0 ∉: a is false") { assertEquals(4.0 ∉: a, false) }
  test("5 ∉: a is true") { assertEquals(5.0 ∉: a, true) }
  test("-2 ∉: a is true") { assertEquals(-2.0 ∉: a, true) }

  val b = oc(2.0, 6.0)
  val c = oo(4.0, 6.0)
  test("a ∩ b is (2.0, 4.0]") { assertEquals(a ∩ b, oc(2.0, 4.0)) }
  test("a ∩ c is e") { assertEquals(a ∩ c, e) }

  test("a ∪ c is [-1.0, 6.0)") { assertEquals(a ∪ c, co(-1.0, 6.0)) }
  test("a ∪ e is a") { assertEquals(a ∪ e, a) }
  test("a ∪ all is all") { assertEquals(a ∪ all, all) }

  test("a \\ all is e") { assertEquals(a \ all, List()) }
  test("a \\ b is [-1.0, 2.0]") { assertEquals(a \ b, List(cc(-1.0, 2.0))) }
  test("a \\ c is a") { assertEquals(a \ c, List(a)) }

  val d = oo(0.0, 3.0)
  test("e ⊂ a is true") { assertEquals(e ⊂ a, true) }
  test("d ⊂ a is true") { assertEquals(d ⊂ a, true) }
  test("b ⊂ a is false") { assertEquals(b ⊂ a, false) }

  test("e ⊃ a is false") { assertEquals(e ⊃ a, false) }
  test("d ⊃ a is false") { assertEquals(d ⊃ a, false) }
  test("b ⊃ a is false") { assertEquals(b ⊃ a, false) }

  test("e ⊆ a is true") { assertEquals(e ⊆ a, true) }
  test("d ⊆ a is true") { assertEquals(d ⊆ a, true) }
  test("b ⊆ a is false") { assertEquals(b ⊆ a, false) }

  test("e ⊇ a is false") { assertEquals(e ⊇ a, false) }
  test("d ⊇ a is false") { assertEquals(d ⊇ a, false) }
  test("b ⊇ a is false") { assertEquals(b ⊇ a, false) }

}
