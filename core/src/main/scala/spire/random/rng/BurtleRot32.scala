package spire
package random
package rng

import java.nio.ByteBuffer
import java.util.Arrays
import java.lang.Integer.rotateLeft

// Contributed by Rex Kerr

/**
 * Bit-mixing random number generator based on rotations from Bob
 * Burtle.  Maintains 16 bytes of state information.  Algorithm from
 * [[http://burtleburtle.net/bob/rand/]]
 */
abstract class BurtleRot32(_a: Int, _b: Int, _c: Int, _d: Int) extends IntBasedGenerator {
  protected var a = _a
  protected var b = _b
  protected var c = _c
  protected var d = _d

  def nextInt: Int = { advance(); d }

  protected def advance(): Unit

  def getSeedBytes: Array[Byte] = {
    val bytes = new Array[Byte](16)
    val bb = ByteBuffer.wrap(bytes)
    bb.putInt(a)
    bb.putInt(b)
    bb.putInt(c)
    bb.putInt(d)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs = if (bytes.length < 16) Arrays.copyOf(bytes, 16) else bytes
    val bb = ByteBuffer.wrap(bs)
    a = bb.getInt()
    b = bb.getInt()
    c = bb.getInt()
    d = bb.getInt()
  }
}

abstract class BurtleCompanion[G <: BurtleRot32] extends GeneratorCompanion[G, Array[Int]] {

  protected def create(_a: Int, _b: Int, _c: Int, _d: Int): G

  def randomSeed: Array[Int] = GlobalRng.generateInts(4)

  def fromBytes(bytes: Array[Byte]): G = {
    val bs = if (bytes.length < 16) Arrays.copyOf(bytes, 16) else bytes
    val bb = ByteBuffer.wrap(bs)
    create(bb.getInt(), bb.getInt(), bb.getInt(), bb.getInt())
  }

  def fromSeed(ints: Array[Int]): G = {
    val zs = if (ints.length < 4) Arrays.copyOf(ints, 4) else ints
    create(zs(0), zs(1), zs(2), zs(3))
  }

  def fromTime(time: Long = System.nanoTime): G = {
    val lcg = Lcg64.fromTime(time)
    create(lcg.nextInt(), lcg.nextInt(), lcg.nextInt(), lcg.nextInt())
  }
}

/**
 * Bit-mixing random number generator based on rotations from Bob
 * Burtle.  Maintains 16 bytes of state information.  Good speed and
 * randomness (see `Burtle3rot` for better randomness).  Algorithm
 * from [[http://burtleburtle.net/bob/rand/]]
 */
final class BurtleRot2(_a: Int, _b: Int, _c: Int, _d: Int) extends BurtleRot32(_a, _b, _c, _d) {
  protected def advance(): Unit = {
    val e = a - rotateLeft(b,27)
    a = b ^ rotateLeft(c,17)
    b = c + d
    c = d + e
    d = e + a
  }

  def copyInit: BurtleRot2 = new BurtleRot2(a, b, c, d)
}

object BurtleRot2 extends BurtleCompanion[BurtleRot2] {
  def create(a: Int, b: Int, c: Int, d: Int): BurtleRot2 = new BurtleRot2(a, b, c, d)
}

/**
 * Bit-mixing random number generator based on rotations from Bob
 * Burtle.  Maintains 16 bytes of state information.  Decent speed and
 * very good randomness (see `Burtle2rot` for better speed).
 * Algorithm from [[http://burtleburtle.net/bob/rand/]]
 */
final class BurtleRot3(_a: Int, _b: Int, _c: Int, _d: Int) extends BurtleRot32(_a, _b, _c, _d) {
  protected def advance(): Unit = {
    val e = a - rotateLeft(b, 23)
    a = b ^ rotateLeft(c, 16)
    b = c + rotateLeft(d, 11)
    c = d + e
    d = e + a
  }

  def copyInit: BurtleRot3 = new BurtleRot3(a, b, c, d)
}

object BurtleRot3 extends BurtleCompanion[BurtleRot3] {
  def create(a: Int, b: Int, c: Int, d: Int): BurtleRot3 = new BurtleRot3(a, b, c, d)
}
