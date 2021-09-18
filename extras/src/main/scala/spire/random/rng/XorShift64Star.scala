package spire
package random
package rng
package extras

import spire.util.Pack

class XorShift64Star(private var seed: Long) extends LongBasedGenerator {
  def copyInit: XorShift64Star = new XorShift64Star(seed)

  override def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)

  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Pack.longFromBytes(bytes)

  def nextLong: Long = {
    seed ^= seed >>> 12
    seed ^= seed << 25
    seed ^= seed >>> 27
    seed * 2685821657736338717L
  }
}

object XorShift64Star extends GeneratorCompanion[XorShift64Star, Long] {
  def randomSeed: Long = System.nanoTime

  def fromSeed(seed: Long): XorShift64Star = {
    assert(seed != 0)
    new XorShift64Star(seed)
  }

  def fromBytes(bytes: Array[Byte]): XorShift64Star = fromSeed(Pack.longFromBytes(bytes))

  def fromTime(time: Long = System.nanoTime): XorShift64Star = fromSeed(time)
}
