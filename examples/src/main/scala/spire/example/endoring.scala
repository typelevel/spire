package spire.example

import spire.math._
import Implicits._


object EndoRingExample extends App {

  // An abelian group is a commutative monoid with an inverse.
  trait AbGroup[A] {
    def append(a: A, b: A): A
    def inverse(a: A): A
    def id: A
  }

  object AbGroup {
    implicit object IntAbGroup extends AbGroup[Int] {
      def append(a: Int, b: Int): Int = a + b
      def inverse(a: Int): Int = -a
      def id: Int = 0
    }

    /**
     * This turns the group of Sets under union into an abelian group by
     * keeping track of the inclusions and exclusions separately. This let's
     * us ensure it is commutative and that we always have an inverse.
     */
    implicit def PairedSetAbGroup[A] = new AbGroup[(Set[A], Set[A])] {
      def append(a: (Set[A], Set[A]), b: (Set[A], Set[A])): (Set[A], Set[A]) =
        ((a._1 -- b._2) union (b._1 -- a._2), (a._2 -- b._1) union (b._2 -- a._1))
      def inverse(a: (Set[A], Set[A])): (Set[A], Set[A]) = (a._2, a._1)
      def id: (Set[A], Set[A]) = (Set.empty, Set.empty)
    }
  }


  implicit def set2pairedSet[A](a: Set[A]): (Set[A], Set[A]) = (a, Set.empty)
  implicit def pairedSet2set[A](a: (Set[A], Set[A])): Set[A] = a._1 -- a._2


  // A (not very strict) endomorphism.
  type Endo[A] = A => A


  /**
   * Toy example of a non-numeric Ring. This constructs the endomorphism ring
   * for an abelian group `ab`. This defines addition as group addition after
   * applying the endomorphism and multiplication as composition.
   */
  trait EndoRing[A] extends Ring[Endo[A]] {
    def ab: AbGroup[A]

    def eq(f: Endo[A], g: Endo[A]): Boolean = throw new UnsupportedOperationException("!!!")

    def plus(f: Endo[A], g: Endo[A]): Endo[A] = a => ab.append(f(a), g(a))
    def negate(f: Endo[A]): Endo[A] = (ab.inverse _) compose f
    def times(f: Endo[A], g: Endo[A]): Endo[A] = f compose g

    /// Identity endomorphism.
    def one: Endo[A] = a => a

    /// Endomorphism to the trivial group.
    def zero: Endo[A] = a => ab.id
  }

  object EndoRing {
    def apply[A](implicit g: AbGroup[A]) = new EndoRing[A] {
      val ab = g
    }
  }



  implicit val intEndoRing = EndoRing[Int]

  // Now we can treat Int => Int functions as a Ring. It obeys all the rules
  // you expect from a Ring, like distributivity. Of course, the
  // "endomorphisms" should map to valid abelian groups. With the Ints, we
  // can multiply by a constant.

  val x2: Int => Int = _ * 2
  val x3: Int => Int = _ * 3
  val inv: Int => Int = -_

  val a = (x2 + inv) * x3
  val b = x2 * x3 + inv * x3

  assert(a(2) == b(2))  // EndoRing is distributive.

  // What's more, we can recreate an Int ring by applying the Endo[Int]
  // with the identity (1).

  val one = Ring[Int => Int].one
  val two = one + one
  val five = two * two + one
  assert(five(1) == 5)
  assert(((five * two) + two)(1) == 12)



  implicit val pairedSetEndoRing = EndoRing[(Set[Int], Set[Int])]

  type PairedSet[A] = (Set[A], Set[A])

  // We can define some simple endomorphisms.
  val id = pairedSetEndoRing.one
  val double: Endo[PairedSet[Int]] = _ map (_ * 2)
  val triple: Endo[PairedSet[Int]] = _ map (_ * 3)
  val inc: Endo[PairedSet[Int]] = _ map (_ + 1)

  // Let's generate the powers of 2 from 0 to n. The endomorphism
  // `double + id` means that we double the elements of a set, then union it
  // with the original.

  val powers: Set[Int] = ((double + id) ** 3)(Set(1))
  assert(powers == Set(1, 2, 4, 8))

  val range: Set[Int] = ((inc + id) ** 4)(Set(1, 11))
  assert(range == Set(1, 2, 3, 4, 5, 11, 12, 13, 14, 15))

  // It's distributive.
  assert(((double + triple) * triple)(Set(1,2,3)) ==
    (double * triple + triple * triple)(Set(1,2,3)))
}


