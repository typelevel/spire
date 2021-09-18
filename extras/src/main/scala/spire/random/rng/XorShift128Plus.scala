package spire
package random
package rng
package extras

import spire.util.Pack

class XorShift128Plus(private var s0: Long, private var s1: Long) extends LongBasedGenerator {
  def copyInit: XorShift128Plus = new XorShift128Plus(s0, s1)

  override def getSeedBytes: Array[Byte] = Pack.longsToBytes(Array(s0, s1))

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val seed = Pack.longsFromBytes(bytes, 2)
    s0 = seed(0)
    s1 = seed(1)
  }

  def nextLong: Long = {
    var x = s0
    val y = s1
    s0 = y
    x ^= x << 23
    s1 = x ^ y ^ (x >>> 17) ^ (y >>> 26)
    s1 + y
  }
}

object XorShift128Plus extends GeneratorCompanion[XorShift128Plus, (Long, Long)] {
  def randomSeed: (Long, Long) = (System.nanoTime, System.nanoTime)

  def fromSeed(seed: (Long, Long)): XorShift128Plus = {
    val (s0, s1) = seed
    assert(s0 != 0 || s1 != 0)
    new XorShift128Plus(s0, s1)
  }

  def fromBytes(bytes: Array[Byte]): XorShift128Plus = {
    val seed = Pack.longsFromBytes(bytes, 2)
    fromSeed((seed(0), seed(1)))
  }

  def fromTime(time: Long = System.nanoTime): XorShift128Plus = fromSeed((time, time))
}
