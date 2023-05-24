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
package example

import spire.algebra._
import spire.implicits._

object EndoRingExample extends App {

  case class PairedSet[A](incl: Set[A], excl: Set[A] = Set.empty[A]) {
    def toSet: Set[A] = incl -- excl
    def map[B](f: A => B): PairedSet[B] =
      PairedSet[B](toSet.map(f))
  }
  object PairedSet {
    def apply[A](elems: A*) =
      new PairedSet(elems.toSet)
  }

  // An abelian group is a commutative monoid with an inverse.
  trait AbGroup[A] extends Group[A]

  object AbGroup {
    implicit object IntAbGroup extends AbGroup[Int] {
      def combine(a: Int, b: Int): Int = a + b
      def inverse(a: Int): Int = -a
      def empty: Int = 0
    }

    /**
     * This turns the group of Sets under union into an abelian group by keeping track of the inclusions and exclusions
     * separately. This let's us ensure it is commutative and that we always have an inverse.
     */
    implicit def PairedSetAbGroup[A]: AbGroup[PairedSet[A]] = new AbGroup[PairedSet[A]] {
      def combine(a: PairedSet[A], b: PairedSet[A]): PairedSet[A] =
        PairedSet((a.incl -- b.excl).union(b.incl -- a.excl), (a.excl -- b.incl).union(b.excl -- a.incl))
      def inverse(a: PairedSet[A]): PairedSet[A] = PairedSet(a.excl, a.incl)
      def empty: PairedSet[A] = PairedSet()
    }
  }

  // A (not very strict) endomorphism.
  type Endo[A] = A => A

  /**
   * Toy example of a non-numeric Ring. This constructs the endomorphism ring for an abelian group `ab`. This defines
   * addition as group addition after applying the endomorphism and multiplication as composition.
   */
  class EndoRing[A: AbGroup] extends Ring[Endo[A]] {
    def plus(f: Endo[A], g: Endo[A]): Endo[A] = a => f(a) |+| g(a)
    def negate(f: Endo[A]): Endo[A] = a => f(a).inverse
    def times(f: Endo[A], g: Endo[A]): Endo[A] = a => f(g(a))

    // Identity endomorphism.
    def one: Endo[A] = a => a

    // Endomorphism to the trivial group.
    def zero: Endo[A] = a => Group[A].empty
  }

  object EndoRing {
    def apply[A: AbGroup] = new EndoRing[A]
  }

  implicit val intEndoRing: EndoRing[Int] = EndoRing[Int]

  // Now we can treat Int => Int functions as a Ring. It obeys all the rules
  // you expect from a Ring, like distributivity. Of course, the
  // "endomorphisms" should map to valid abelian groups. With the Ints, we
  // can multiply by a constant.

  val x2: Int => Int = _ * 2
  val x3: Int => Int = _ * 3
  val inv: Int => Int = -_

  val a = (x2 + inv) * x3
  val b = (x2 * x3) + (inv * x3)

  (0 until 10).foreach(i => assert(a(i) == b(i))) // EndoRing is distributive.

  // What's more, we can recreate an Int ring by applying the Endo[Int]
  // with the id (1).

  val one = Ring[Int => Int].one
  val two = one + one
  val five = two * two + one
  (0 until 10).foreach { i =>
    assert(five(i) == 5 * i)
    assert(((five * two) + two)(i) == 12 * i)
  }

  implicit val pairedSetEndoRing: EndoRing[PairedSet[Int]] = EndoRing[PairedSet[Int]]

  // We can define some simple endomorphisms.
  val id = pairedSetEndoRing.one
  val double: Endo[PairedSet[Int]] = _.map(_ * 2)
  val triple: Endo[PairedSet[Int]] = _.map(_ * 3)
  val inc: Endo[PairedSet[Int]] = _.map(_ + 1)

  // Let's generate the powers of 2 from 0 to n. The endomorphism
  // `double + id` means that we double the elements of a set, then union it
  // with the original.
  val powers: Set[Int] = ((double + id) ** 3)(PairedSet(1)).toSet
  assert(powers == Set(1, 2, 4, 8))

  val range: Set[Int] = ((inc + id) ** 4)(PairedSet(1, 11)).toSet
  assert(range == Set(1, 2, 3, 4, 5, 11, 12, 13, 14, 15))

  // It's distributive.
  val q = PairedSet(1, 2, 3)
  val e1 = (double + triple) * triple
  val e2 = (double * triple) + (triple * triple)
  assert(e1(q) == e2(q))
}
