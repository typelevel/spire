package spire.random

import spire.math._

final class MmixPrng(_seed: Long) extends LongGenerator {
  private var seed: Long = _seed

  def copy: Generator = new MmixPrng(seed)

  def nextLong(): Long = {
    val next: Long = 6364136223846793005L * seed + 1442695040888963407L
    seed = next
    next.toLong
  }
}

object MmixPrng {
  def apply() = new MmixPrng(scala.util.Random.nextLong)
  def apply(seed: Long) = new MmixPrng(seed)
}
