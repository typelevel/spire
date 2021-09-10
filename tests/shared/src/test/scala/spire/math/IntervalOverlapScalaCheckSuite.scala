package spire
package math

import spire.implicits._
import spire.laws.arb.{interval => interval_, rational}

import interval.Overlap._
import org.scalacheck.Prop._

class IntervalOverlapScalaCheckSuite extends munit.ScalaCheckSuite {
  //
  // property("(x overlap y) = (y overlap x)") {
  //   forAll { (x: Interval[Rational], y: Interval[Rational]) =>
  //     x.overlap(y) == y.overlap(x)
  //   }
  // }
  //
  // property("x overlap x = Equal(x, x)") {
  //   forAll { (x: Interval[Rational]) =>
  //     x.overlap(x) == Equal[Rational]()
  //   }
  // }
  //
  // property("(x overlap Ø) = Subset(Ø, x) id x != Ø") {
  //   forAll { (x: Interval[Rational]) =>
  //     (x.nonEmpty) ==> {
  //       val empty = Interval.empty[Rational]
  //       x.overlap(empty) == Subset(empty, x)
  //     }
  //   }
  // }
  //
  // property("consistency with Interval#isSubset") {
  //   forAll { (x: Interval[Rational], y: Interval[Rational]) =>
  //     x.overlap(y).isSubset == (x.isSubsetOf(y) || y.isSubsetOf(x))
  //   }
  // }
  //
  // property("(-inf, a] overlap [a, +inf) = PartialOverlap") {
  //   forAll { (x: Rational) =>
  //     Interval.atOrBelow(x).overlap(Interval.atOrAbove(x)) match {
  //       case _: PartialOverlap[_] => true
  //       case _                    => false
  //     }
  //   }
  // }
  //
  // property("[a, c) overlap (b, d] = PartialOverlap if a < b < c < d") {
  //   forAll { (x: Rational, y: Rational, m: Rational, n: Rational) =>
  //     /* TODO: the name `catsKernel` leaks here (and below), OK? */
  //     import spire.algebra.Order.catsKernelOrderingForOrder
  //
  //     val sorted = List(x, y, m, n).sorted
  //     (sorted.distinct == sorted) ==> {
  //       Interval.openUpper(sorted(0), sorted(2)).overlap(Interval.openLower(sorted(1), sorted(3))) match {
  //         case _: PartialOverlap[_] => true
  //         case _                    => false
  //       }
  //     }
  //   }
  // }
  //
  // property("[a, c] overlap [b, d] = PartialOverlap if a < b <= c < d") {
  //   forAll { (x: Rational, y: Rational, m: Rational, n: Rational) =>
  //
  //     import spire.algebra.Order.catsKernelOrderingForOrder
  //
  //     val sorted = List(x, y, m, n).sorted
  //     (sorted.distinct.size >= 3 && sorted(0) != sorted(1) && sorted(2) != sorted(3)) ==> {
  //       Interval.closed(sorted(0), sorted(2)).overlap(Interval.closed(sorted(1), sorted(3))) match {
  //         case _: PartialOverlap[_] => true
  //         case _                    => false
  //       }
  //     }
  //   }
  // }
  //
  // property("(-inf, a) overlap (b, +inf) = PartialOverlap if a > b") {
  //   forAll { (x: Rational, y: Rational) =>
  //     (x != y) ==> {
  //       Interval.below(max(x, y)).overlap(Interval.above(min(x, y))) match {
  //         case _: PartialOverlap[_] => true
  //         case _                    => false
  //       }
  //     }
  //   }
  // }
  //
  // property("(-inf, a) overlap (b, +inf) = Disjoint if a <= b") {
  //   forAll { (x: Rational, y: Rational) =>
  //     Interval.below(min(x, y)).overlap(Interval.above(max(x, y))).isDisjoint
  //   }
  // }
  //
  // property("Disjoint((-inf, a), (b, +inf)).join = [a, b]") {
  //   forAll { (x: Rational, y: Rational) =>
  //     val l = min(x, y)
  //     val u = max(x, y)
  //     Disjoint(Interval.below(l), Interval.above(u)).join == Interval.closed(l, u)
  //   }
  // }
  //
  // property("[a, b) overlap (c, d] = Disjoint if a < b <= c < d") {
  //   forAll { (x: Rational, y: Rational, m: Rational, n: Rational) =>
  //
  //     import spire.algebra.Order.catsKernelOrderingForOrder
  //
  //     val sorted = List(x, y, m, n).sorted
  //     (sorted(0) < sorted(1) && sorted(2) < sorted(3)) ==> {
  //       val overlap = Interval.openUpper(sorted(0), sorted(1)).overlap(Interval.openLower(sorted(2), sorted(3)))
  //       overlap.isDisjoint &&
  //       overlap.asInstanceOf[Disjoint[Rational]].join == Interval.closed(sorted(1), sorted(2))
  //     }
  //   }
  // }
  //
  // property("[a, b] overlap [c, d] = Disjoint if a <= b < c <= d") {
  //   forAll { (x: Rational, y: Rational, m: Rational, n: Rational) =>
  //
  //     import spire.algebra.Order.catsKernelOrderingForOrder
  //
  //     val sorted = List(x, y, m, n).sorted
  //     (sorted(1) < sorted(2)) ==> {
  //       val overlap = Interval.closed(sorted(0), sorted(1)).overlap(Interval.closed(sorted(2), sorted(3)))
  //       overlap.isDisjoint &&
  //       overlap.asInstanceOf[Disjoint[Rational]].join == Interval.open(sorted(1), sorted(2))
  //     }
  //   }
  // }
  //
  // property("x overlap [a] is never a PartialOverlap") {
  //   forAll { (x: Interval[Rational], b: Rational) =>
  //     x.overlap(Interval.point(b)) match {
  //       case _: PartialOverlap[_] => false
  //       case _                    => true
  //     }
  //   }
  // }
}
