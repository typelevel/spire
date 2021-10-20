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
package random
package rng
package extras

import spire.util.Pack

class XorShift64Star(private var seed: Long) extends LongBasedGenerator {
  def copyInit: XorShift64Star = new XorShift64Star(seed)

  override def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)

  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Pack.longFromBytes(bytes)

  def nextLong(): Long = {
    seed ^= seed >>> 12
    seed ^= seed << 25
    seed ^= seed >>> 27
    seed * 2685821657736338717L
  }
}

object XorShift64Star extends GeneratorCompanion[XorShift64Star, Long] {
  def randomSeed(): Long = System.nanoTime

  def fromSeed(seed: Long): XorShift64Star = {
    assert(seed != 0)
    new XorShift64Star(seed)
  }

  def fromBytes(bytes: Array[Byte]): XorShift64Star = fromSeed(Pack.longFromBytes(bytes))

  def fromTime(time: Long = System.nanoTime): XorShift64Star = fromSeed(time)
}
