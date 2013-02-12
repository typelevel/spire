package spire.random

final class MmixPrng(_seed: Long) extends Generator {
  private var seed: Long = _seed

  def copy: Generator = new MmixPrng(seed)

  def nextInt(): Int = (nextLong >>> 32).toInt

  override def nextLong(): Long = {
    val next: Long = 6364136223846793005L * seed + 1442695040888963407L
    seed = next
    next
  }

  override def nextDouble(): Double = {
    val n = nextLong()
    ((((n >>> 38) & 0xffffffff) << 27) + ((n & 0xffffffffL) >>> 5)) * 1.1102230246251565e-16
  }
}

object MmixPrng {
  def apply() = new MmixPrng(scala.util.Random.nextLong)
  def apply(seed: Long) = new MmixPrng(seed)
}
