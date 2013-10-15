package spire.random
package mutable

import spire.util.Pack
import spire.math._
import java.nio.ByteBuffer

final class Lcg64(_seed: Long) extends LongBasedGenerator {
  private var seed: Long = _seed

  def copy: Lcg64 = new Lcg64(seed)

  def getSeed: Long = seed

  def setSeed(n: Long): Unit = seed = n

  def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)

  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Pack.longFromBytes(bytes)

  def nextLong(): Long = {
    seed = 6364136223846793005L * seed + 1442695040888963407L
    seed
  }
}

object Lcg64 extends GeneratorCompanion[Lcg64, Long] {
  def randomSeed(): Long = System.nanoTime()

  def fromBytes(bytes: Array[Byte]): Lcg64 = new Lcg64(Pack.longFromBytes(bytes))
  def fromSeed(seed: Long): Lcg64 = new Lcg64(seed)
  def fromTime(time: Long = System.nanoTime()): Lcg64 = new Lcg64(time)

  def step(n: Long): Long = 6364136223846793005L * n + 1442695040888963407L
}
