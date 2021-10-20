/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
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

/**
 * A trait for linearly ordered additive commutative monoid. The following laws holds:
 *
 * (1) if `a <= b` then `a + c <= b + c` (linear order), (2) `signum(x) = -1` if `x < 0`, `signum(x) = 1` if `x > 0`,
 * `signum(x) = 0` otherwise,
 *
 * Negative elements only appear when `scalar` is a additive abelian group, and then (3) `abs(x) = -x` if `x < 0`, or
 * `x` otherwise,
 *
 * Laws (1) and (2) lead to the triange inequality:
 *
 * (4) `abs(a + b) <= abs(a) + abs(b)`
 *
 * Signed should never be extended in implementations, rather the AdditiveCMonoid and AdditiveAbGroup subtraits. We
 * cannot use self-types to express the constraint `self: AdditiveCMonoid =>` (interaction with specialization?).
 */
trait Signed[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Order[A] {

  /**
   * Returns Zero if `a` is 0, Positive if `a` is positive, and Negative is `a` is negative.
   */
  def sign(a: A): Sign = Sign(signum(a))

  /**
   * Returns 0 if `a` is 0, 1 if `a` is positive, and -1 is `a` is negative.
   */
  def signum(a: A): Int

  /**
   * An idempotent function that ensures an object has a non-negative sign.
   */
  def abs(a: A): A

  def isSignZero(a: A): Boolean = signum(a) == 0
  def isSignPositive(a: A): Boolean = signum(a) > 0
  def isSignNegative(a: A): Boolean = signum(a) < 0

  def isSignNonZero(a: A): Boolean = signum(a) != 0
  def isSignNonPositive(a: A): Boolean = signum(a) <= 0
  def isSignNonNegative(a: A): Boolean = signum(a) >= 0
}

trait SignedAdditiveCMonoid[@sp(Byte, Short, Int, Long, Float, Double) A]
    extends Any
    with Signed[A]
    with AdditiveCMonoid[A] {

  /**
   * Returns 0 if `a` is 0, 1 if `a` is positive, and -1 is `a` is negative.
   */
  def signum(a: A): Int = {
    val c = compare(a, zero)
    if (c < 0) -1
    else if (c > 0) 1
    else 0
  }
}

trait SignedAdditiveAbGroup[@sp(Byte, Short, Int, Long, Float, Double) A]
    extends Any
    with SignedAdditiveCMonoid[A]
    with AdditiveAbGroup[A] {
  def abs(a: A): A = if (compare(a, zero) < 0) negate(a) else a
}

object Signed {
  def apply[A](implicit s: Signed[A]): Signed[A] = s
}
