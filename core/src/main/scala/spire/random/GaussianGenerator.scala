package spire.random

import scala.annotation.tailrec

import java.lang.Math

class GaussianGenerator(gen: Generator) extends Generator {
  private[random] var extra: Boolean = false
  private[random] var value: Double = 0.0

  def copy: GaussianGenerator = {
    val gg = new GaussianGenerator(gen.copy)
    gg.extra = extra
    gg.value = value
    gg
  }

  def getSeedBytes(): Array[Byte] = gen.getSeedBytes()

  def setSeedBytes(bytes: Array[Byte]) {
    extra = false
    gen.setSeedBytes(bytes)
  }

  def nextInt(): Int = gen.nextInt()

  def nextLong(): Long = gen.nextLong()

  def nextGaussian(): Double = if (extra) {
    extra = false
    value
  } else {
    @tailrec def loop(x: Double, y: Double): Double = {
      val s = x * x + y * y
      if (s >= 1.0 || s == 0.0) {
        loop(nextDouble() * 2 - 1, nextDouble() * 2 - 1)
      } else {
        val scale = Math.sqrt(-2.0 * Math.log(s) / s)
        extra = true
        value = y * scale
        x * scale
      }
    }
    loop(nextDouble() * 2 - 1, nextDouble() * 2 - 1)
  }

  def nextGaussian(mean: Double, stddev: Double): Double =
    nextGaussian() * stddev + mean

  def fillGaussians(arr: Array[Double]): Unit =
    fillGaussians(arr, 0.0, 1.0)

  def fillGaussians(arr: Array[Double], mean: Double, stddev: Double) {
    var i = 0
    val len = arr.length & 0xfffffffe

    @tailrec def loop(i: Int, x: Double, y: Double) {
      val s = x * x + y * y
      if (s >= 1.0 || s == 0.0) {
        loop(i, nextDouble() * 2 - 1, nextDouble() * 2 - 1)
      } else {
        val scale = Math.sqrt(-2.0 * Math.log(s) / s)
        arr(i) = x * scale * stddev + mean
        arr(i + 1) = y * scale * stddev + mean
      }
    }

    while (i < len) {
      loop(i, nextDouble() * 2 - 1, nextDouble() * 2 - 1)
      i += 2
    }

    if (len < arr.length) arr(len) = nextGaussian() * stddev + mean
  }

  def generateGaussians(n: Int): Array[Double] = {
    val arr = new Array[Double](n)
    fillGaussians(arr)
    arr
  }

  def generateGaussians(n: Int, mean: Double, stddev: Double): Array[Double] = {
    val arr = new Array[Double](n)
    fillGaussians(arr, mean, stddev)
    arr
  }
}

object GaussianGenerator {
  def apply(gen: Generator) = new GaussianGenerator(gen)
}
