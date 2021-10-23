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
package std

import spire.algebra._
import scala.util.NotGiven

trait ArrayInstances0 {
  type NI0[A] = NotGiven[VectorSpace[Array[A], A]]

  implicit def ArrayCModule[@sp(Int, Long, Float, Double) A: NI0: ClassTag: CRing]: CModule[Array[A], A] =
    new ArrayCModule[A]
}

trait ArrayInstances1 extends ArrayInstances0 {
  type NI1[A] = NotGiven[NormedVectorSpace[Array[A], A]]

  implicit def ArrayVectorSpace[@sp(Int, Long, Float, Double) A: NI1: ClassTag: Field]: VectorSpace[Array[A], A] =
    new ArrayVectorSpace[A]

  implicit def ArrayEq[@sp A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
}

@SerialVersionUID(0L)
final private class ArrayCModule[@sp(Int, Long, Float, Double) A: ClassTag: CRing](implicit
  nvs: NotGiven[VectorSpace[Array[A], A]]
) extends CModule[Array[A], A]
    with Serializable {
  def scalar: CRing[A] = CRing[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

@SerialVersionUID(0L)
final private class ArrayVectorSpace[@sp(Int, Float, Long, Double) A: ClassTag: Field](implicit
  nnvs: NotGiven[NormedVectorSpace[Array[A], A]]
) extends VectorSpace[Array[A], A]
    with Serializable {
  def scalar: Field[A] = Field[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}
