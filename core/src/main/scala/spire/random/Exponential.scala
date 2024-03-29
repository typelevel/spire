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
package random

trait Exponential[@sp(Float, Double) A] extends Any {

  /**
   * Return an `A` that has an exponential distribution from 0, with the given `rate` parameter.
   */
  def apply(rate: A): Dist[A]
}

object Exponential extends ExponentialInstances {
  @inline final def apply[@sp(Float, Double) A](implicit e: Exponential[A]): Exponential[A] = e

  def apply[@sp(Float, Double) A](rate: A)(implicit e: Exponential[A]): Dist[A] = e(rate)
}

trait ExponentialInstances {

  implicit val float: Exponential[Float] =
    new Exponential[Float] {
      def apply(rate: Float): Dist[Float] =
        new DistFromGen(g => (Ziggurat.rexp(g) / rate).toFloat)
    }

  implicit val double: Exponential[Double] =
    new Exponential[Double] {
      def apply(rate: Double): Dist[Double] =
        new DistFromGen(g => Ziggurat.rexp(g) / rate)
    }
}
