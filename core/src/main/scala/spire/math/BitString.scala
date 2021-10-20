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
package math

import spire.algebra.Bool

trait BitString[@sp(Byte, Short, Int, Long) A] extends Any with Bool[A] {
  def signed: Boolean
  def width: Int
  def toHexString(n: A): String

  def bitCount(n: A): Int
  def highestOneBit(n: A): A
  def lowestOneBit(n: A): A
  def numberOfLeadingZeros(n: A): Int
  def numberOfTrailingZeros(n: A): Int

  def leftShift(n: A, i: Int): A
  def rightShift(n: A, i: Int): A
  def signedRightShift(n: A, i: Int): A
  def rotateLeft(n: A, i: Int): A
  def rotateRight(n: A, i: Int): A
}

object BitString {
  def apply[A](implicit ev: BitString[A]): BitString[A] = ev
}
