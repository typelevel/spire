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

final class SyncGenerator(gen: Generator) extends Generator {
  def copyInit: SyncGenerator = new SyncGenerator(gen.copy)

  override def sync: SyncGenerator = this

  def getSeedBytes: Array[Byte] = gen.getSeedBytes

  def setSeedBytes(bytes: Array[Byte]): Unit = gen.setSeedBytes(bytes)

  def nextInt(): Int = this.synchronized { gen.nextInt() }

  def nextLong(): Long = this.synchronized { gen.nextLong() }
}

object SyncGenerator {
  def apply(gen: Generator): SyncGenerator = new SyncGenerator(gen)
}
