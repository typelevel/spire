package spire.random
package rng

import java.nio.ByteBuffer
import java.util.Arrays

final class Cmwc5(_x: Long, _y: Long, _z: Long, _w: Long, _v: Long) extends LongBasedGenerator {
  private var x: Long = _x
  private var y: Long = _y
  private var z: Long = _z
  private var w: Long = _w
  private var v: Long = _v

  def copyInit: Cmwc5 = new Cmwc5(x, y, z, w, v)

  def getSeed: Array[Long] = {
    val longs = new Array[Long](5)
    longs(0) = x
    longs(1) = y
    longs(2) = z
    longs(3) = w
    longs(4) = v
    longs
  }

  def setSeed(longs: Array[Long]) {
    x = longs(0)
    y = longs(1)
    z = longs(2)
    w = longs(3)
    v = longs(4)
  }

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](40)
    val bb = ByteBuffer.wrap(bytes)
    bb.putLong(x)
    bb.putLong(y)
    bb.putLong(z)
    bb.putLong(w)
    bb.putLong(v)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]) {
    val bs = if (bytes.length < 40) Arrays.copyOf(bytes, 40) else bytes
    val bb = ByteBuffer.wrap(bs)
    x = bb.getLong()
    y = bb.getLong()
    z = bb.getLong()
    w = bb.getLong()
    v = bb.getLong()
  }

  def nextLong(): Long = {
    val t: Long = x ^ (x >>> 7)
    x = y
    y = z
    z = w
    w = v
    v = (v ^ (v << 6)) ^ (t ^ (t << 13))
    (y + y + 1) * v
  }
}

object Cmwc5 extends GeneratorCompanion[Cmwc5, Array[Long]] {
  def randomSeed(): Array[Long] = GlobalRng.generateLongs(5)

  def fromBytes(bytes: Array[Byte]): Cmwc5 = {
    val bs = if (bytes.length < 40) Arrays.copyOf(bytes, 40) else bytes
    val bb = ByteBuffer.wrap(bytes)
    val x = bb.getLong()
    val y = bb.getLong()
    val z = bb.getLong()
    val w = bb.getLong()
    val v = bb.getLong()
    new Cmwc5(x, y, z, w, v)
  }

  def fromSeed(seed: Array[Long]): Cmwc5 = {
    val zs = if (seed.length < 5) Arrays.copyOf(seed, 5) else seed
    new Cmwc5(zs(0), zs(1), zs(2), zs(3), zs(4))
  }

  def fromTime(time: Long = System.nanoTime()): Cmwc5 = {
    val lcg = Lcg64.fromTime(time)
    new Cmwc5(lcg.nextLong(), lcg.nextLong(), lcg.nextLong(), lcg.nextLong(), lcg.nextLong())
  }
}
