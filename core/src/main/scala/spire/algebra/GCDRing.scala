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

package spire.algebra

import scala.{specialized => sp}

/**
 * GCDRing implements a GCD ring.
 *
 * For two elements x and y in a GCD ring, we can choose two elements d and m such that:
 *
 * d = gcd(x, y) m = lcm(x, y)
 *
 * d * m = x * y
 *
 * Additionally, we require:
 *
 * gcd(0, 0) = 0 lcm(x, 0) = lcm(0, x) = 0
 *
 * and commutativity:
 *
 * gcd(x, y) = gcd(y, x) lcm(x, y) = lcm(y, x)
 */
trait GCDRing[@sp(Int, Long, Float, Double) A] extends Any with CRing[A] {
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A
}

trait GCDRingFunctions[R[T] <: GCDRing[T]] extends _root_.algebra.ring.RingFunctions[R] {
  def gcd[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A], eqA: Eq[A]): A =
    ev.gcd(a, b)(eqA)
  def lcm[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A], eqA: Eq[A]): A =
    ev.lcm(a, b)(eqA)
}

object GCDRing extends GCDRingFunctions[GCDRing] {
  @inline final def apply[A](implicit ev: GCDRing[A]): GCDRing[A] = ev
}
