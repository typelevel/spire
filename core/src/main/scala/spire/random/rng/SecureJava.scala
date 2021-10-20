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

import java.security.SecureRandom

class SecureJava(rand: SecureRandom) extends IntBasedGenerator {
  def copyInit: SecureJava = new SecureJava(rand)

  override def getSeedBytes: Array[Byte] =
    throw new UnsupportedOperationException("getSeedBytes")

  def setSeedBytes(bytes: Array[Byte]): Unit =
    throw new UnsupportedOperationException("setSeedBytes")

  def nextInt(): Int = rand.nextInt
}

object SecureJava {

  @deprecated("seed is ignored except on windows. will be removed before 1.0", "0.12.0")
  def fromBytes(bytes: Array[Byte]): SecureJava =
    new SecureJava(new SecureRandom(bytes))

  def apply: SecureJava =
    new SecureJava(new SecureRandom)
}
