package spire.math.extras.interval

import spire.implicits._

class IntervalSeqSuite extends munit.FunSuite {

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
    assert(empty[Int] == (a & c))
    assert(atOrAbove(1) == (a ^ c))
    assert(hole(1) == (a | d))
    assert(above(1) == (a & d))
    assert(below(1) == (a ^ d))
  }

  test("atIsSameAsApply") {
    val is = above(1)
    assert(is.at(1) == is.apply(1))
  }

  test("equalsSameType") {
    val is = above(1)
    assert(is != "DOH!")
  }

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
    assert((a ^ b) == algebra.xor(a, b))
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
    assert(true)
  }
}
