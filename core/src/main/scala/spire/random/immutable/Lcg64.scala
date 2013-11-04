package spire.random.immutable

import spire.util.Pack

final case class Lcg64(seed: Long) extends LongBasedGenerator {
  def getSeedBytes: Array[Byte] = Pack.longToBytes(seed)

  def withSeedBytes(bytes: Array[Byte]): Generator = new Lcg64(Pack.longFromBytes(bytes))

  def nextLong: (Lcg64, Long) = {
    val seed0 = 6364136223846793005L * seed + 1442695040888963407L
    (new Lcg64(seed0), seed0)
  }
}

object Lcg64 extends GeneratorCompanion[Lcg64, Long] {
  def fromBytes(bytes: Array[Byte]): Lcg64 = new Lcg64(Pack.longFromBytes(bytes))
  def fromSeed(seed: Long): Lcg64 = new Lcg64(seed)
}
