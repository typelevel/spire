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

class IntervalReciprocalSuite extends munit.FunSuite {

  def t(a: Interval[Rational], b: Interval[Rational]): Unit =
    test(s"[1]/$a = $b") { assertEquals(a.reciprocal, b) }

  def error(a: Interval[Rational]): Unit =
    test(s"[1]/$a = error") {
      intercept[ArithmeticException] { a.reciprocal }
    }

  // point(x)
  t(Interval.point(r"1/5"), Interval.point(r"5"))
  t(Interval.point(r"-99"), Interval.point(r"-1/99"))
  error(Interval.point(r"0"))

  // above(x)
  t(Interval.above(r"3"), Interval.open(r"0", r"1/3"))
  t(Interval.above(r"0"), Interval.above(r"0")) //fixme
  error(Interval.above(r"-1"))

  // atOrAbove(x)
  t(Interval.atOrAbove(r"1/9"), Interval.openLower(r"0", r"9"))
  error(Interval.atOrAbove(r"0"))
  error(Interval.atOrAbove(r"-2"))

  // closed(x, y)
  t(Interval.closed(r"1/2", r"4"), Interval.closed(r"1/4", r"2"))
  error(Interval.closed(r"0", r"6"))
  error(Interval.closed(r"-2", r"1/5"))
  error(Interval.closed(r"-1/9", r"0"))
  t(Interval.closed(r"-70", r"-14"), Interval.closed(r"-1/14", r"-1/70"))

  // openLower(x, y)
  t(Interval.openLower(r"1/2", r"4"), Interval.openUpper(r"1/4", r"2"))
  t(Interval.openLower(r"0", r"6"), Interval.atOrAbove(r"1/6")) //fixme
  error(Interval.openLower(r"-2", r"1/5"))
  error(Interval.openLower(r"-1/9", r"0"))
  t(Interval.openLower(r"-70", r"-14"), Interval.openUpper(r"-1/14", r"-1/70"))

  // openUpper(x, y)
  t(Interval.openUpper(r"1/2", r"4"), Interval.openLower(r"1/4", r"2"))
  error(Interval.openUpper(r"0", r"6"))
  error(Interval.openUpper(r"-2", r"1/5"))
  t(Interval.openUpper(r"-1/9", r"0"), Interval.atOrBelow(r"-9")) //fixme
  t(Interval.openUpper(r"-70", r"-14"), Interval.openLower(r"-1/14", r"-1/70"))

  // open
  t(Interval.open(r"1/2", r"4"), Interval.open(r"1/4", r"2"))
  t(Interval.open(r"0", r"6"), Interval.above(r"1/6")) //fixme
  error(Interval.open(r"-2", r"1/5"))
  t(Interval.open(r"-1/9", r"0"), Interval.below(r"-9")) //fixme
  t(Interval.open(r"-70", r"-14"), Interval.open(r"-1/14", r"-1/70"))

  // below(x)
  error(Interval.below(r"3"))
  t(Interval.below(r"0"), Interval.below(r"0")) //fixme
  t(Interval.below(r"-1"), Interval.open(r"-1", r"0")) //fixme

  // atOrBelow(x)
  error(Interval.atOrBelow(r"1/9"))
  error(Interval.atOrBelow(r"0"))
  t(Interval.atOrBelow(r"-2"), Interval.openUpper(r"-1/2", r"0")) //fixme
}
