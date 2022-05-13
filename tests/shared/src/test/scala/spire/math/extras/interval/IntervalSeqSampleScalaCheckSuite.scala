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

import org.scalacheck.Prop._
import spire.math.Rational
import spire.std.any._
import spire.syntax.all._

class IntervalSeqSampleScalaCheckSuite extends munit.ScalaCheckSuite {

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  import IntervalSeqArbitrary._

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a: IntervalSeq[Int], r: IntervalSeq[Int], op: Boolean => Boolean) = {
    val support = a.edges.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.above(value) === op(a.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a: IntervalSeq[Int],
                       b: IntervalSeq[Int],
                       r: IntervalSeq[Int],
                       op: (Boolean, Boolean) => Boolean
  ) = {
    val support = (a.edges ++ b.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a: IntervalSeq[Int],
                        b: IntervalSeq[Int],
                        c: IntervalSeq[Int],
                        r: IntervalTrie[Long],
                        op: (Boolean, Boolean, Boolean) => Boolean
  ) = {
    val support = (a.edges ++ b.edges ++ c.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value), c.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value), c.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value), c.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  property("sample_not") {
    forAll { (a: IntervalSeq[Int]) =>
      unarySampleTest(a, ~a, ~_)
    }
  }

  property("sample_and") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      binarySampleTest(a, b, a & b, _ & _)
    }
  }

  property("sample_or") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      binarySampleTest(a, b, a | b, _ | _)
    }
  }

  property("sample_xor") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      binarySampleTest(a, b, a ^ b, _ ^ _)
    }
  }

  property("toStringParse") {
    forAll { (a0: IntervalSeq[Int]) =>
      // first convert the interval of long to an interval of rationals, since that is what parse returns
      val rationalIntervals = a0.intervals.map(_.mapBounds(Rational.apply))
      val a: IntervalSeq[Rational] = rationalIntervals.foldLeft(IntervalSeq.empty[Rational])(_ | IntervalSeq(_))
      // then do the roundtrip test like with IntervalSet
      val aText = a.toString
      val b = IntervalSeq(aText)
      a == b
    }
  }

  property("isContiguous") {
    forAll { (a: IntervalSeq[Int]) =>
      a.isContiguous == a.intervals.size <= 1
    }
  }

  property("hull") {
    forAll { (a: IntervalSeq[Int]) =>
      val hullSet = IntervalSeq(a.hull)
      val outside = ~hullSet
      val nothingOutside = (a & outside) == IntervalSeq.empty[Int]
      val allInside = a.intervals.forall(i => hullSet.isSupersetOf(IntervalSeq(i)))
      nothingOutside & allInside
    }
  }

  /**
   * Check optimized intersects method against naive implementation using &
   */
  property("intersects/intersection") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      val r1 = a.intersects(b)
      val r2 = !(a & b).isEmpty
      r1 == r2
    }
  }

  /**
   * Check optimized isSupersetOf method against naive implementation using &
   */
  property("isSupersetOf/intersection") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      val r1 = a.isSupersetOf(b)
      val r2 = (a & b) == b
      r1 == r2
    }
  }

  property("isSupersetOf") {
    forAll { (a: IntervalSeq[Int], x: Int) =>
      val b = a & IntervalSeq.atOrAbove(x)
      a.isSupersetOf(b)
    }
  }

  property("disjoint") {
    forAll { (s: IntervalSeq[Int], x: Int) =>
      val a = s & IntervalSeq.below(x)
      val b = s & IntervalSeq.atOrAbove(x)
      !a.intersects(b)
    }
  }

  property("equals/hashCode") {
    forAll { (a: IntervalSeq[Int], b: IntervalSeq[Int]) =>
      if (a == b) a.hashCode == b.hashCode else true
    }
  }

  property("iterator") {
    forAll { (a: IntervalSeq[Int]) =>
      a.intervalIterator.toIndexedSeq == a.intervals.toIndexedSeq
    }
  }
}
