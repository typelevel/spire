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
package rng

import spire.util.Pack

final class Lcg64(_seed: Long) extends LongBasedGenerator {
  private var seed: Long = _seed

  def copyInit: Lcg64 = new Lcg64(seed)

  def getSeed: Long = seed

  def setSeed(n: Long): Unit = seed = n

  override def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)

  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Pack.longFromBytes(bytes)

  def nextLong(): Long = {
    seed = 6364136223846793005L * seed + 1442695040888963407L
    seed
  }
}

object Lcg64 extends GeneratorCompanion[Lcg64, Long] {
  def randomSeed(): Long = System.nanoTime

  def fromBytes(bytes: Array[Byte]): Lcg64 = new Lcg64(Pack.longFromBytes(bytes))
  def fromSeed(seed: Long): Lcg64 = new Lcg64(seed)
  def fromTime(time: Long = System.nanoTime): Lcg64 = new Lcg64(time)

  def step(n: Long): Long = 6364136223846793005L * n + 1442695040888963407L
}
