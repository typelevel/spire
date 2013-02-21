package spire.random

import java.nio.ByteBuffer

object Pack {
  def longToBytes(n: Long): Array[Byte] = {
    val arr = new Array[Byte](8)
    arr(0) = ((n >>> 56) & 0xff).toByte
    arr(1) = ((n >>> 48) & 0xff).toByte
    arr(2) = ((n >>> 40) & 0xff).toByte
    arr(3) = ((n >>> 32) & 0xff).toByte
    arr(4) = ((n >>> 24) & 0xff).toByte
    arr(5) = ((n >>> 16) & 0xff).toByte
    arr(6) = ((n >>> 8) & 0xff).toByte
    arr(7) = (n & 0xff).toByte
    arr
  }

  def intToBytes(n: Long): Array[Byte] = {
    val arr = new Array[Byte](4)
    arr(0) = ((n >>> 24) & 0xff).toByte
    arr(1) = ((n >>> 16) & 0xff).toByte
    arr(2) = ((n >>> 8) & 0xff).toByte
    arr(3) = (n & 0xff).toByte
    arr
  }

  def intsToBytes(ints: Array[Int]): Array[Byte] = {
    val len = ints.length
    val arr = new Array[Byte](len * 4)
    var i = 0
    var j = 0
    while (i < len) {
      val n = arr(i)
      arr(j) = ((n >>> 24) & 0xff).toByte
      arr(j + 1) = ((n >>> 16) & 0xff).toByte
      arr(j + 2) = ((n >>> 8) & 0xff).toByte
      arr(j + 3) = (n & 0xff).toByte
      i += 1
      j += 4
    }
    arr
  }

  def longsToBytes(ints: Array[Long]): Array[Byte] = {
    val len = ints.length
    val arr = new Array[Byte](len * 8)
    var i = 0
    var j = 0
    while (i < len) {
      val n = arr(i)
      arr(j) = ((n >>> 56) & 0xff).toByte
      arr(j + 1) = ((n >>> 48) & 0xff).toByte
      arr(j + 2) = ((n >>> 40) & 0xff).toByte
      arr(j + 3) = ((n >>> 32) & 0xff).toByte
      arr(j + 4) = ((n >>> 24) & 0xff).toByte
      arr(j + 5) = ((n >>> 16) & 0xff).toByte
      arr(j + 6) = ((n >>> 8) & 0xff).toByte
      arr(j + 7) = (n & 0xff).toByte
      i += 1
      j += 8
    }
    arr
  }

  def intFromBytes(bytes: Array[Byte]): Int =
    intFromByteBuffer(ByteBuffer.wrap(bytes))

  def intFromByteBuffer(bb: ByteBuffer): Int =
    if (bb.remaining >= 4) {
      bb.getInt()
    } else {
      var n = 0
      while (bb.remaining > 0) n = (n << 8) | bb.get
      n
    }

  def longFromBytes(bytes: Array[Byte]): Long =
    longFromByteBuffer(ByteBuffer.wrap(bytes))

  def longFromByteBuffer(bb: ByteBuffer): Long =
    if (bb.remaining >= 8) {
      bb.getLong()
    } else {
      var n = 0L
      while (bb.remaining > 0) n = (n << 8) | bb.get
      n
    }

  def bytesFromByteBuffer(bb: ByteBuffer, n: Int): Array[Byte] = {
    val out = new Array[Byte](n)
    if (bb.remaining >= n) {
      bb.get(out)
    } else {
      var i = 0
      while (bb.remaining > 0) { out(i) = bb.get; i += 1 }
    }
    out
  }

  def intsFromBytes(bytes: Array[Byte], n: Int): Array[Int] =
    intsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  def intsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Int] = {
    val out = new Array[Int](n)
    var i = 0
    while (i < n && bb.remaining >= 4) { out(i) = bb.getInt(); i += 1 }
    if (i < n && bb.remaining > 0) out(i) = intFromByteBuffer(bb)
    out
  }

  def longsFromBytes(bytes: Array[Byte], n: Int): Array[Long] =
    longsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  def longsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Long] = {
    val out = new Array[Long](n)
    var i = 0
    while (i < n && bb.remaining >= 8) { out(i) = bb.getLong(); i += 1 }
    if (i < n && bb.remaining > 0) out(i) = longFromByteBuffer(bb)
    out
  }
}
