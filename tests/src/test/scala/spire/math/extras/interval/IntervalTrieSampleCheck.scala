package spire.math.extras.interval

import org.scalacheck.Prop._
import org.scalacheck.Properties
import spire.std.any._
import spire.syntax.all._

object IntervalTrieSampleCheck extends Properties("IntervalSet.Sample") {

  // this will resolve to the Arbitrary instance for Boolean from scalacheck
  import IntervalTrieArbitrary._

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def unarySampleTest(a:IntervalTrie[Long], r:IntervalTrie[Long], op:Boolean => Boolean) = {
    val support = a.edges.toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value))
      val sameAt = r.at(value) === op(a.at(value))
      val sameAfter = r.above(value) === op(a.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def binarySampleTest(a:IntervalTrie[Long], b:IntervalTrie[Long], r:IntervalTrie[Long], op:(Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  // a test that works by sampling the result at all relevant places and checks consistency with the boolean operation
  def trinarySampleTest(a:IntervalTrie[Long], b:IntervalTrie[Long], c:IntervalTrie[Long], r:IntervalTrie[Long], op:(Boolean, Boolean, Boolean) => Boolean) = {
    val support = (a.edges ++ b.edges ++ c.edges).toArray.sorted.distinct
    support.forall { value =>
      val sameBefore = r.below(value) === op(a.below(value), b.below(value), c.below(value))
      val sameAt = r.at(value) === op(a.at(value), b.at(value), c.at(value))
      val sameAfter = r.above(value) === op(a.above(value), b.above(value), c.above(value))
      sameBefore & sameAt & sameAfter
    }
  }

  property("sample_not") = forAll { a: IntervalTrie[Long] =>
    unarySampleTest(a, ~a, ~_)
  }

  property("sample_and") = forAll { (a: IntervalTrie[Long], b: IntervalTrie[Long]) =>
    binarySampleTest(a, b, a & b, _ & _)
  }

  property("sample_or") = forAll { (a: IntervalTrie[Long], b: IntervalTrie[Long]) =>
    binarySampleTest(a, b, a | b, _ | _)
  }

  property("sample_xor") = forAll { (a: IntervalTrie[Long], b: IntervalTrie[Long]) =>
    binarySampleTest(a, b, a ^ b, _ ^ _)
  }

  property("toStringParse") = forAll { a: IntervalTrie[Long] =>
    val aText = a.toString
    val b = IntervalTrie(aText)
    a == b
  }

  property("isContiguous") = forAll { a: IntervalTrie[Long] =>
    a.isContiguous == (a.intervals.size <= 1)
  }

  property("hull") = forAll { a: IntervalTrie[Long] =>
    val hullSet = IntervalTrie(a.hull)
    val outside = ~hullSet
    val nothingOutside = (a & outside) == IntervalTrie.empty[Long]
    val allInside = a.intervals.forall(i => hullSet.isSupersetOf(IntervalTrie(i)))
    nothingOutside & allInside
  }

  /**
   * Check optimized intersects method against naive implementation using &
   */
  property("intersects/intersection") = forAll { (a: IntervalTrie[Long], b: IntervalTrie[Long]) =>
    val r1 = a intersects b
    val r2 = !(a & b).isEmpty
    r1 == r2
  }

  /**
   * Check optimized isSupersetOf method against naive implementation using &
   */
  property("isSupersetOf/intersection") = forAll { (a: IntervalTrie[Long], b: IntervalTrie[Long]) =>
    val r1 = a isSupersetOf b
    val r2 = (a & b) == b
    r1 == r2
  }

  property("isSupersetOf") = forAll { (a: IntervalTrie[Long], x: Long) =>
    val b = a & IntervalTrie.atOrAbove(x)
    a isSupersetOf b
  }

  property("disjoint") = forAll { (s: IntervalTrie[Long], x: Long) =>
    val a = s & IntervalTrie.below(x)
    val b = s & IntervalTrie.atOrAbove(x)
    !(a intersects b)
  }

  property("iterator") = forAll { a: IntervalTrie[Long] =>
    a.intervalIterator.toIndexedSeq == a.intervals.toIndexedSeq
  }
}
