package spire
package random
package rng
package extras

import spire.syntax.cfor._
import java.nio.ByteBuffer
import java.util.Arrays

class XorShift1024Star(private val s: Array[Long], private var p: Int) extends LongBasedGenerator {
  import XorShift1024Star.{BYTES, N}

  def copyInit: XorShift1024Star = new XorShift1024Star(s.clone, p)

  def getSeedBytes: Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    cfor(0)(_ < N, _ + 1) { i => bb.putLong(s(i)) }
    bb.putInt(p)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    cfor(0)(_ < N, _ + 1) { i => s(i) = bb.getLong() }
    p = bb.getInt
  }

  def nextLong(): Long = {
    val s0 = s(p)
    p = (p + 1) & 15
    var s1 = s(p)
    s1 ^= s1 << 31
    s(p) = s1 ^ s0 ^ (s1 >>> 11) ^ (s0 >>> 30)
    s(p) * 1181783497276652981L
  }
}

object XorShift1024Star extends GeneratorCompanion[XorShift1024Star, (Array[Long], Int)] {
  @inline private val N = 16
  @inline private val BYTES = N * 8 + 4

  def randomSeed(): (Array[Long], Int) = (Utils.seedFromLong(N, Utils.longFromTime()), 0)

  def fromSeed(seed: (Array[Long], Int)): XorShift1024Star =
    seed match {
      case (s, p) =>
        assert(s.length == N && p < N && s.exists(_ != 0))
        new XorShift1024Star(s, p)
    }

  def fromBytes(bytes: Array[Byte]): XorShift1024Star = {
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    val s = new Array[Long](N)

    cfor(0)(_ < N, _ + 1) { i => s(i) = bb.getLong() }
    val p = bb.getInt

    fromSeed((s, p))
  }

  def fromTime(time: Long = System.nanoTime): XorShift1024Star =
    fromSeed((Utils.seedFromLong(N, Utils.longFromTime(time)), 0))
}
