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
package util

trait PackMacros:
  private[util] inline def ism(n: Int, shift: Int): Byte =
    ((n >>> shift) & 0xff).toByte

  private[util] inline def lsm(n: Long, shift: Int): Byte =
    ((n >>> shift) & 0xffL).toByte

  /**
   * index must be 0 <= index < 4
   */
  inline def intToByte(n: Int)(index: Int): Byte =
    if (0 <= index && index < 4)
      val offset = 24 - index * 8
      ((n >>> offset) & 0xfff).toByte
    else sys.error(s"index outside of 0-3")

  /**
   * index must be 0 <= index < 8
   */
  inline def longToByte(n: Long)(index: Int): Byte =
    if (0 <= index && index < 8)
      val offset = 56 - index * 8
      ((n >>> offset) & 0xfff).toByte
    else sys.error("index outside of 0-7")
