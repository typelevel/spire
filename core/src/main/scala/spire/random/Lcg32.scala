package spire.random

// Linear congruent RNG with 32-bits of state.
// Numbers as in Numerical Recipes in C, 2nd ed.
// Contributed by Rex Kerr.
class Lcg32(_seed: Int) extends IntBasedGenerator {
  private var seed: Int = _seed

  def copy: Lcg32 = new Lcg32(seed)

  def getSeedBytes(): Array[Byte] = Util.intToBytes(seed)

  def setSeedBytes(bytes: Array[Byte]): Unit = seed = Util.intFromBytes(bytes)

  def nextInt(): Int = {
    seed = 1664525 * seed + 1013904223
    seed
  }
}

object Lcg32 extends GeneratorCompanion[Lcg32, Int] {
  def randomSeed(): Int = System.nanoTime.toInt

  def fromBytes(bytes: Array[Byte]): Lcg32 = new Lcg32(Util.intFromBytes(bytes))
  def fromSeed(seed: Int): Lcg32 = new Lcg32(seed)
  def fromTime(time: Long = System.nanoTime) = new Lcg32(time.toInt)

  def step(n: Int): Int = 1664525 * n + 1013904223
}
