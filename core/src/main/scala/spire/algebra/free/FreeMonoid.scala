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
package algebra
package free

final class FreeMonoid[A] private (val terms: List[A]) extends AnyVal { lhs =>

  /**
   * Map each term to type `B` and sum them using `B`'s [[Semigroup]], as long as there is at least 1 term. Otherwise,
   * return `None`.
   */
  def runSemigroup[B](f: A => B)(implicit B: Semigroup[B]): Option[B] =
    B.combineAllOption(terms.iterator.map(f))

  /**
   * Map each term to type `B` and sum them using `B`'s [[Monoid]].
   */
  def run[B](f: A => B)(implicit B: Monoid[B]): B =
    B.combineAll(terms.iterator.map(f))

  def |+|(rhs: FreeMonoid[A]): FreeMonoid[A] =
    new FreeMonoid(lhs.terms ::: rhs.terms)

  override def toString: String =
    terms.mkString(" |+| ")
}

object FreeMonoid { companion =>
  final def empty[A]: FreeMonoid[A] = new FreeMonoid(Nil)

  final def apply[A](a: A): FreeMonoid[A] = lift(a)
  final def lift[A](a: A): FreeMonoid[A] = new FreeMonoid[A](a :: Nil)

  implicit def FreeMonoidMonoid[A]: Monoid[FreeMonoid[A]] = new Monoid[FreeMonoid[A]] {
    def empty: FreeMonoid[A] = companion.empty
    def combine(a: FreeMonoid[A], b: FreeMonoid[A]): FreeMonoid[A] = a |+| b

    override def combineAll(as: IterableOnce[FreeMonoid[A]]): FreeMonoid[A] = {
      val b = List.newBuilder[A]
      as.iterator.foreach(b ++= _.terms)
      new FreeMonoid(b.result())
    }
  }
}
