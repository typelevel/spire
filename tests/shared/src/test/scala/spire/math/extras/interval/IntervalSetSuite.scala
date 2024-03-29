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

class IntervalSetSuite extends munit.FunSuite {

  import IntervalSeq._

  test("leafOperation") {
    val a = above(1)
    val b = atOrAbove(1)
    val c = point(1)
    val d = hole(1)
    assert(atOrAbove(1) == (a | b))
    assert(above(1) == (a & b))
    assert(point(1) == (a ^ b))
    assert(atOrAbove(1) == (a | c))
    assert(empty[Long] == (a & c))
    assert(atOrAbove(1) == (a ^ c))
    assert(hole(1) == (a | d))
    assert(above(1) == (a & d))
    assert(below(1) == (a ^ d))
  }

  test("atIsSameAsApply") {
    val is = above(1)
    assert(is.at(1) == is.apply(1))
  }

  // This doesn't work in scala 3 but seems a useless test
  // test("equalsDifferentType") {
  //   val is = above(1)
  //   assert(is != "DOH!")
  // }

  test("subsetof") {
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
    assert((a ^ b) == algebra.xor(a, b))
  }

  test("iteratorAfterEnd") {
    intercept[NoSuchElementException] {
      val all = IntervalSeq.empty[Int]
      val it = all.intervalIterator
      it.next()
    }
  }

  test("illegalStateHull1") {
    intercept[IllegalStateException] {
      val t = IntervalSeq.above(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
  }

  test("illegalStateHull2") {
    intercept[IllegalStateException] {
      val t = IntervalSeq.below(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
  }

  test("illegalStateIterator1") {
    intercept[IllegalStateException] {
      val t = IntervalSeq.above(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
  }

  test("illegalStateIterator2") {
    intercept[IllegalStateException] {
      val t = IntervalSeq.below(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
  }
}
