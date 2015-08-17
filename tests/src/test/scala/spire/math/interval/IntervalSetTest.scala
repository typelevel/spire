package spire.math.interval

import org.scalatest.FunSuite
import spire.implicits._

class IntervalSetTest extends FunSuite {

  import IntervalSet._

  test("leafOperation") {
    val a = above(1L)
    val b = atOrAbove(1L)
    val c = point(1L)
    val d = hole(1L)
    assert(atOrAbove(1L) == (a | b))
    assert(above(1L) == (a & b))
    assert(point(1L) == (a ^ b))
    assert(atOrAbove(1L) == (a | c))
    assert(empty[Long] == (a & c))
    assert(atOrAbove(1L) == (a ^ c))
    assert(hole(1L) == (a | d))
    assert(above(1L) == (a & d))
    assert(below(1L) == (a ^ d))
  }

  test("atIsSameAsApply") {
    val is = above(1)
    assert(is.at(1) == is.apply(1))
  }

  test("equalsDifferentType") {
    val is = above(1)
    assert(is != "DOH!")
  }

  test("subsetof") {
    assert(above(1).isSupersetOf(above(1)))
    assert(atOrAbove(1).isSupersetOf(above(1)))
    assert(!above(1).isSupersetOf(atOrAbove(1)))

    assert(!above(1).isProperSupersetOf(above(1)))
    assert(atOrAbove(1).isProperSupersetOf(above(1)))
    assert(!above(1).isProperSupersetOf(atOrAbove(1)))
  }

  test("algebra") {
    val algebra = IntervalSetAlgebra.booleanAlgebra[Long]
    val a = IntervalSet.above(1L)
    val b = IntervalSet.below(1L)
    assert((a ^ b) == algebra.xor(a, b))
  }

  test("iteratorAfterEnd") {
    intercept[NoSuchElementException] {
      val all = IntervalSet.empty[Int]
      val it = all.intervalIterator
      it.next()
    }
    assert(true)
  }

  test("illegalStateHull1") {
    intercept[IllegalStateException] {
      val t = IntervalSet.above(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
    assert(true)
  }

  test("illegalStateHull2") {
    intercept[IllegalStateException] {
      val t = IntervalSet.below(1)
      t.kindsAccessor(0) = 9
      t.hull
    }
    assert(true)
  }

  test("illegalStateIterator1") {
    intercept[IllegalStateException] {
      val t = IntervalSet.above(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
    assert(true)
  }

  test("illegalStateIterator2") {
    intercept[IllegalStateException] {
      val t = IntervalSet.below(1)
      t.kindsAccessor(0) = 9
      t.intervalIterator.next()
    }
    assert(true)
  }
}
