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

package spire.math.extras.interval

import spire.implicits._

class IntervalSeqSuite extends munit.FunSuite {

  import IntervalSeq._

  test("leafOperation") {
    val a = above(1)
    val b = atOrAbove(1)
    val c = point(1)
    val d = hole(1)
    assertEquals(atOrAbove(1), (a | b))
    assertEquals(above(1), (a & b))
    assertEquals(point(1), (a ^ b))
    assertEquals(atOrAbove(1), (a | c))
    assertEquals(empty[Int], (a & c))
    assertEquals(atOrAbove(1), (a ^ c))
    assertEquals(hole(1), (a | d))
    assertEquals(above(1), (a & d))
    assertEquals(below(1), (a ^ d))
  }

  test("atIsSameAsApply") {
    val is = above(1)
    assertEquals(is.at(1), is.apply(1))
  }

  // This doesn't work in scala 3 but seems a useless test
  // test("equalsSameType") {
  //   val is = above(1)
  //   assert(is != "DOH!")
  // }

  test("subsetOf") {
    assert(above(1).isSupersetOf(above(1)))
    assert(atOrAbove(1).isSupersetOf(above(1)))
    assert(!above(1).isSupersetOf(atOrAbove(1)))

    assert(!above(1).isProperSupersetOf(above(1)))
    assert(atOrAbove(1).isProperSupersetOf(above(1)))
    assert(!above(1).isProperSupersetOf(atOrAbove(1)))
  }

  test("algebra") {
    val algebra = IntervalSeq.algebra[Int]
    val a = IntervalSeq.above(1)
    val b = IntervalSeq.below(1)
    assertEquals((a ^ b), algebra.xor(a, b))
  }

  test("coverage") {
    intercept[NoSuchElementException] {
      val all = IntervalSeq.empty[Int]
      val it = all.intervalIterator
      it.next()
    }
    intercept[IllegalStateException] {
      val t = IntervalSeq.above(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
    intercept[IllegalStateException] {
      val t = IntervalSeq.below(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
    intercept[IllegalStateException] {
      val t = IntervalSeq.above(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
    intercept[IllegalStateException] {
      val t = IntervalSeq.below(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
  }
}
