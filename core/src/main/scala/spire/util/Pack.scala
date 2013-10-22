package spire.util

import java.nio.ByteBuffer

object Pack {
	def longToBytes(n: Long): Array[Byte] = {
		val arr = new Array[Byte](8)
		arr(0) = longToByte(n, 0)
		arr(1) = longToByte(n, 1)
		arr(2) = longToByte(n, 2)
		arr(3) = longToByte(n, 3)
		arr(4) = longToByte(n, 4)
		arr(5) = longToByte(n, 5)
		arr(6) = longToByte(n, 6)
		arr(7) = longToByte(n, 7)
		arr
	}

	def intToBytes(n: Int): Array[Byte] = {
		val arr = new Array[Byte](4)
		arr(0) = intToByte(n, 0)
		arr(1) = intToByte(n, 1)
		arr(2) = intToByte(n, 2)
		arr(3) = intToByte(n, 3)
		arr
	}

	def longToByte(n: Long, index: Byte): Byte = ((n >>> (64 - ((index + 1) * 8))) & 0xff).toByte

	def intToByte(n: Int, index: Byte): Byte = ((n >>> (32 - ((index + 1) * 8))) & 0xff).toByte

	def intsToBytes(ints: Array[Int]): Array[Byte] = {
		val len = ints.length
		val arr = new Array[Byte](len * 4)
		var i = 0
		var j = 0
		while (i < len) {
			val n = arr(i)
			arr(j) = intToByte(n, 0)
			arr(j + 1) = intToByte(n, 1)
			arr(j + 2) = intToByte(n, 2)
			arr(j + 3) = intToByte(n, 3)
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
			arr(j) = longToByte(n, 0)
			arr(j + 1) = longToByte(n, 1)
			arr(j + 2) = longToByte(n, 2)
			arr(j + 3) = longToByte(n, 3)
			arr(j + 4) = longToByte(n, 4)
			arr(j + 5) = longToByte(n, 5)
			arr(j + 6) = longToByte(n, 6)
			arr(j + 7) = longToByte(n, 7)
			i += 1
			j += 8
		}
		arr
	}

	def intFromBytes(bytes: Array[Byte]): Int =
		intFromByteBuffer(ByteBuffer.wrap(bytes))

	def intFromBytes(b1: Byte, b2: Byte, b3: Byte, b4: Byte) =
		b1 << 24 |
			b2 << 16 |
			b3 << 8 |
			b4

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

	def longFromBytes(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte) =
		b1 << 56 |
			b2 << 48 |
			b3 << 40 |
			b4 << 32 |
			b5 << 24 |
			b6 << 16 |
			b7 << 8 |
			b8

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
			while (bb.remaining > 0) {out(i) = bb.get; i += 1}
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
