package spire.random
package rng

import spire.util.Pack
import spire.math._
import java.nio.ByteBuffer

final class Serial(seed0: Long) extends LongBasedGenerator {
  private[this] var seed: Long = seed0
  def copyInit: Serial = new Serial(seed)
  def getSeed: Long = seed
  def setSeed(n: Long): Unit = seed = n
  def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)
  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Pack.longFromBytes(bytes)
  def nextLong(): Long = { seed += 1; seed }
}

object Serial extends GeneratorCompanion[Serial, Long] {
  def randomSeed(): Long = System.nanoTime()
  def fromBytes(bytes: Array[Byte]): Serial = new Serial(Pack.longFromBytes(bytes))
  def fromSeed(seed: Long): Serial = new Serial(seed)
  def fromTime(time: Long = System.nanoTime()): Serial = new Serial(time)
}
