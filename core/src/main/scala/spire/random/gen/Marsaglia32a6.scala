package spire.random
package gen

import java.nio.ByteBuffer
import java.util.Arrays

// Contributed by Rex Kerr

/**
 * Marsaglia "Weyl sequence" RNG with cycle length of 2^192^ - 2^32^
 * from [[http://www.jstatsoft.org/v08/i14/paper]]. Quite fast and
 * quite random; requires 24 bytes of state.
 */
class Marsaglia32a6(_x: Int, _y: Int, _z: Int, _w: Int, _v: Int, _d: Int) extends IntBasedGenerator {
  private[this] var x: Int = _x
  private[this] var y: Int = _y
  private[this] var z: Int = _z
  private[this] var w: Int = _w
  private[this] var v: Int = _v
  private[this] var d: Int = _d

  def copyInit: Marsaglia32a6 = new Marsaglia32a6(x, y, z, w, v, d)

  def getSeed: Array[Int] = {
    val ints = new Array[Int](6)
    ints(0) = x
    ints(1) = y
    ints(2) = z
    ints(3) = w
    ints(4) = v
    ints(5) = d
    ints
  }

  def setSeed(seed: Array[Int]) {
    val zs = if (seed.length < 6) Arrays.copyOf(seed, 6) else seed
    x = zs(0)
    y = zs(0)
    z = zs(0)
    w = zs(0)
    v = zs(0)
    d = zs(0)
  }

  def getSeedBytes: Array[Byte] = {
    val bytes = new Array[Byte](24)
    val bb = ByteBuffer.wrap(bytes)
    bb.putInt(x)
    bb.putInt(y)
    bb.putInt(z)
    bb.putInt(w)
    bb.putInt(v)
    bb.putInt(d)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]) {
    val bs = if (bytes.length < 24) Arrays.copyOf(bytes, 24) else bytes
    val bb = ByteBuffer.wrap(bs)
    x = bb.getInt()
    y = bb.getInt()
    z = bb.getInt()
    w = bb.getInt()
    v = bb.getInt()
    d = bb.getInt()
  }

  def nextInt() = {
    val t = x ^ (x >>> 2)
    x = y
    y = z
    z = w
    w = v
    v = v ^ (v << 4) ^ (t ^ (t << 1))
    d += 362437
    d + v
  }
}

object Marsaglia32a6 extends GeneratorCompanion[Marsaglia32a6, Array[Int]] {
  def fromBytes(bytes: Array[Byte]): Marsaglia32a6 = {
    val bs = if (bytes.length < 24) Arrays.copyOf(bytes, 24) else bytes
    val bb = ByteBuffer.wrap(bytes)
    val x = bb.getInt()
    val y = bb.getInt()
    val z = bb.getInt()
    val w = bb.getInt()
    val v = bb.getInt()
    val d = bb.getInt()
    new Marsaglia32a6(x, y, z, w, v, d)
  }

  def fromSeed(ints: Array[Int]): Marsaglia32a6 = {
    val zs = if (ints.length < 6) Arrays.copyOf(ints, 6) else ints
    new Marsaglia32a6(zs(0), zs(1), zs(2), zs(3), zs(4), zs(5))
  }

  def fromTime(time: Long = System.nanoTime): Marsaglia32a6 = {
    val lcg = Lcg64.fromTime(time)
    val x = lcg.nextInt()
    val y = lcg.nextInt()
    val z = lcg.nextInt()
    val w = lcg.nextInt()
    val v = lcg.nextInt()
    val d = lcg.nextInt()
    new Marsaglia32a6(x, y, z, w, v, d)
  }

  def randomSeed: Array[Int] = GlobalRng.generateInts(6)
}
