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
package util

import java.nio.ByteBuffer

/**
 * These methods are all big-endian.
 *
 * That is, bytes[0] is the most-significant byte.
 */
object Pack extends PackMacros {

  def intToBytes(n: Int): Array[Byte] = {
    val arr = new Array[Byte](4)
    arr(0) = ism(n, 24)
    arr(1) = ism(n, 16)
    arr(2) = ism(n, 8)
    arr(3) = ism(n, 0)
    arr
  }

  def intsToBytes(ints: Array[Int]): Array[Byte] = {
    val arr = new Array[Byte](ints.length * 4)
    var i = 0
    var j = 0
    while (i < ints.length) {
      val n = ints(i)
      arr(j) = ism(n, 24)
      arr(j + 1) = ism(n, 16)
      arr(j + 2) = ism(n, 8)
      arr(j + 3) = ism(n, 0)
      i += 1
      j += 4
    }
    arr
  }

  def intFromBytes(bytes: Array[Byte]): Int =
    intFromByteBuffer(ByteBuffer.wrap(bytes))

  def intFromBytes(b1: Byte, b2: Byte, b3: Byte, b4: Byte): Int =
    (b1 & 0xff) << 24 | (b2 & 0xff) << 16 | (b3 & 0xff) << 8 | b4 & 0xff

  def intFromByteBuffer(bb: ByteBuffer): Int =
    if (bb.remaining >= 4) {
      bb.getInt()
    } else {
      var n = 0
      while (bb.remaining > 0) n = n << 8 | bb.get
      n
    }

  def intsFromBytes(bytes: Array[Byte], n: Int): Array[Int] =
    intsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  def intsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Int] = {
    val out = new Array[Int](n)
    var i = 0
    while (i < n && bb.remaining >= 4) {
      out(i) = bb.getInt();
      i += 1
    }
    if (i < n && bb.remaining > 0) out(i) = intFromByteBuffer(bb)
    out
  }

  def longToBytes(n: Long): Array[Byte] = {
    val arr = new Array[Byte](8)
    arr(0) = lsm(n, 56)
    arr(1) = lsm(n, 48)
    arr(2) = lsm(n, 40)
    arr(3) = lsm(n, 32)
    arr(4) = lsm(n, 24)
    arr(5) = lsm(n, 16)
    arr(6) = lsm(n, 8)
    arr(7) = lsm(n, 0)
    arr
  }

  def longsToBytes(longs: Array[Long]): Array[Byte] = {
    val arr = new Array[Byte](longs.length * 8)
    var i = 0
    var j = 0
    while (i < longs.length) {
      val n = longs(i)
      arr(j) = lsm(n, 56)
      arr(j + 1) = lsm(n, 48)
      arr(j + 2) = lsm(n, 40)
      arr(j + 3) = lsm(n, 32)
      arr(j + 4) = lsm(n, 24)
      arr(j + 5) = lsm(n, 16)
      arr(j + 6) = lsm(n, 8)
      arr(j + 7) = lsm(n, 0)
      i += 1
      j += 8
    }
    arr
  }

  def longFromBytes(bytes: Array[Byte]): Long =
    longFromByteBuffer(ByteBuffer.wrap(bytes))

  def longFromBytes(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte): Long =
    (b1 & 0xffL) << 56 | (b2 & 0xffL) << 48 | (b3 & 0xffL) << 40 |
      (b4 & 0xffL) << 32 | (b5 & 0xffL) << 24 | (b6 & 0xffL) << 16 |
      (b7 & 0xffL) << 8 | b8 & 0xffL

  def longFromByteBuffer(bb: ByteBuffer): Long =
    if (bb.remaining >= 8) {
      bb.getLong()
    } else {
      var n = 0L
      while (bb.remaining > 0) n = n << 8 | bb.get
      n
    }

  def longsFromBytes(bytes: Array[Byte], n: Int): Array[Long] =
    longsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  def longsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Long] = {
    val out = new Array[Long](n)
    var i = 0
    while (i < n && bb.remaining >= 8) {
      out(i) = bb.getLong();
      i += 1
    }
    if (i < n && bb.remaining > 0) out(i) = longFromByteBuffer(bb)
    out
  }

  def bytesFromByteBuffer(bb: ByteBuffer, n: Int): Array[Byte] = {
    val out = new Array[Byte](n)
    if (bb.remaining >= n) {
      bb.get(out)
    } else {
      var i = 0
      while (bb.remaining > 0) {
        out(i) = bb.get;
        i += 1
      }
    }
    out
  }

  // macro stuff beyond this point

  def intToByteRuntime(n: Int)(index: Int): Byte =
    if (0 <= index && index < 4) {
      (n >>> 24 - index * 8 & 0xff).toByte
    } else {
      throw new IllegalArgumentException(s"$index outside of 0-3")
    }

  def longToByteRuntime(n: Long)(index: Int): Byte =
    if (0 <= index && index < 8) {
      (n >>> 56 - index * 8 & 0xff).toByte
    } else {
      throw new IllegalArgumentException(s"$index outside of 0-7")
    }

}
