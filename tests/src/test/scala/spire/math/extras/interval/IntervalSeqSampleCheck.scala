package spire.math.extras.interval

import org.scalacheck.Prop._
import org.scalacheck.Properties
import spire.math.Rational
import spire.std.any._
import spire.syntax.all._

object IntervalSeqSampleCheck extends Properties("IntervalSeq.Sample") {

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  import IntervalSeqArbitrary._

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a:IntervalSeq[Long], r:IntervalSeq[Long], op:Boolean => Boolean) = {
    val support = a.edges.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.above(value) === op(a.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a:IntervalSeq[Long], b:IntervalSeq[Long], r:IntervalSeq[Long], op:(Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a:IntervalSeq[Long], b:IntervalSeq[Long], c:IntervalSeq[Long], r:IntervalTrie[Long], op:(Boolean, Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges ++ c.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value), c.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value), c.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value), c.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  property("sample_not") = forAll { a: IntervalSeq[Long] =>
    unarySampleTest(a, ~a, ~_)
  }

  property("sample_and") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    binarySampleTest(a, b, a & b, _ & _)
  }

  property("sample_or") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    binarySampleTest(a, b, a | b, _ | _)
  }

  property("sample_xor") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    binarySampleTest(a, b, a ^ b, _ ^ _)
  }

  property("toStringParse") = forAll { a0: IntervalSeq[Long] =>
    // first convert the interval of long to an interval of rationals, since that is what parse returns
    val rationalIntervals = a0.intervals.map(_.mapBounds(Rational.apply))
    val a : IntervalSeq[Rational] = (IntervalSeq.empty[Rational] /: rationalIntervals)(_ | IntervalSeq(_))
    // then do the roundtrip test like with IntervalSet
    val aText = a.toString
    val b = IntervalSeq(aText)
    a == b
  }

  property("isContiguous") = forAll { a: IntervalSeq[Long] =>
    a.isContiguous == (a.intervals.size <= 1)
  }

  property("hull") = forAll { a: IntervalSeq[Long] =>
    val hullSet = IntervalSeq(a.hull)
    val outside = ~hullSet
    val nothingOutside = (a & outside) == IntervalSeq.empty[Long]
    val allInside = a.intervals.forall(i => hullSet.isSupersetOf(IntervalSeq(i)))
    nothingOutside & allInside
  }

  /**
   * Check optimized intersects method against naive implementation using &
   */
  property("intersects/intersection") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    val r1 = a intersects b
    val r2 = !(a & b).isEmpty
    r1 == r2
  }

  /**
   * Check optimized isSupersetOf method against naive implementation using &
   */
  property("isSupersetOf/intersection") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    val r1 = a isSupersetOf b
    val r2 = (a & b) == b
    r1 == r2
  }

  property("isSupersetOf") = forAll { (a: IntervalSeq[Long], x: Long) =>
    val b = a & IntervalSeq.atOrAbove(x)
    a isSupersetOf b
  }

  property("disjoint") = forAll { (s: IntervalSeq[Long], x: Long) =>
    val a = s & IntervalSeq.below(x)
    val b = s & IntervalSeq.atOrAbove(x)
    !(a intersects b)
  }

  property("equals/hashCode") = forAll { (a: IntervalSeq[Long], b: IntervalSeq[Long]) =>
    if(a==b) a.hashCode == b.hashCode else true
  }

  property("iterator") = forAll { a: IntervalSeq[Long] =>
    a.intervalIterator.toIndexedSeq == a.intervals.toIndexedSeq
  }
}