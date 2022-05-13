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
package math.prime

import spire.syntax.cfor._

/**
 * Fast BitSet implementation.
 *
 * This bitset is just intended to be a little bit faster than Scala's, and to support accessing its internals, which we
 * do in some cases.
 *
 * The max length (~2B) is a current limit to how big individual sieve segments can get. Until our sieving is more
 * efficient, we don't want segments that big anyway, so this is OK.
 */
object BitSet {
  def alloc(length: Int): BitSet =
    new BitSet(length, new Array[Int](length >>> 5))
}

case class BitSet(length: Int, array: Array[Int]) {

  def +=(n: Int): Unit = {
    val q = n >>> 5
    array(q) = array(q) | 1 << (n & 31)
  }

  def -=(n: Int): Unit = {
    val q = n >>> 5
    array(q) = array(q) & ~(1 << (n & 31))
  }

  def update(n: Int, b: Boolean): Unit =
    if (b) this += n else this -= n

  def apply(n: Int): Boolean =
    (array(n >>> 5) >>> (n & 31) & 1) == 1

  def clear(): Unit =
    cfor(0)(_ < array.length, _ + 1)(array(_) = 0)
}
