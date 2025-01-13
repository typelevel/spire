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

// NOTE: fpow vs pow is a bit of a trainwreck :P
// overloading is evil, but it's definitely what users will expect.

/**
 * This is a type class for types with n-roots. The value returned by `nroot` and `sqrt` are only guaranteed to be
 * approximate answers (except in the case of `Real`).
 *
 * Also, generally `nroot` s where `n` is even are not defined for negative numbers. The behaviour is undefined if this
 * is attempted. It would be nice to ensure an exception is raised, but some types may defer computation and testing if
 * a value is negative may not be ideal. So, do not count on `ArithmeticException` s to save you from bad arithmetic!
 */
trait NRoot[@sp(Double, Float, Int, Long) A] extends Any {
  def nroot(a: A, n: Int): A
  def sqrt(a: A): A = nroot(a, 2)
  def fpow(a: A, b: A): A
}

object NRoot {
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: NRoot[A]): NRoot[A] = ev
}
