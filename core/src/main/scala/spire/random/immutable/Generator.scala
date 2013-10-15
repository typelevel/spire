package spire.random
package immutable

import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

/**
 * An immutable random number generator. This provides few methods, as the
 * preferred way to work with this is through [[Dist]].
 */
trait Generator {
  def getSeedBytes: Array[Byte]

  def withSeedBytes(bytes: Array[Byte]): Generator

  /**
   * Generate an equally-distributed random Int.
   */
  def nextInt: (Generator, Int)

  /**
   * Generates a random long. All 64-bit long values are equally likely.
   */
  def nextLong: (Generator, Long)

  def nextFloat: (Generator, Float) = {
    val (gen, x) = nextInt
    (gen, (x >>> 8) * 5.9604645e-8f)
  }

  def nextDouble: (Generator, Double) = {
    val (gen, x) = nextLong
    (gen, (x >>> 11) * 1.1102230246251565e-16)
  }

  def nextGaussian: (Generator, Double) = {
    @tailrec def loop(gen0: Generator): (Generator, Double) = {
      val (gen1, x0) = gen0.nextDouble
      val (gen2, y0) = gen1.nextDouble
      val x = x0 * 2 - 1
      val y = y0 * 2 - 1
      val s = x * x + y * y
      if (s >= 1.0 || s == 0.0) {
        loop(gen2)
      } else {
        val scale = Math.sqrt(-2.0 * Math.log(s) / s)
        (new FixedGaussianGenerator(y * scale, gen2), x * scale)
      }
    }
    loop(this)
  }

  def nextGaussian(mean: Double, stddev: Double): (Generator, Double) = {
    val (gen, x) = nextGaussian
    (gen, x * stddev + mean)
  }

  def toMutable: mutable.Generator = new MutableWrapper(this)
}

abstract class IntBasedGenerator extends Generator { self =>
  def nextLong: (Generator, Long) = {
    val (gen0, low) = nextInt
    val (gen1, high) = nextInt
    (gen1, ((high & 0xFFFFFFFFL) << 32) | (low & 0xFFFFFFFFL))
  }
}

abstract class LongBasedGenerator extends Generator { self =>
  def nextInt: (Generator, Int) = {
    val (gen, x) = nextLong
    (gen, (x >>> 32).toInt)
  }
}

private final class FixedGaussianGenerator(gaussian: Double, gen: Generator) extends Generator {
  def getSeedBytes: Array[Byte] = gen.getSeedBytes

  def withSeedBytes(bytes: Array[Byte]): Generator = gen.withSeedBytes(bytes)

  /**
   * Generate an equally-distributed random Int.
   */
  def nextInt: (Generator, Int) = {
    val (gen0, x) = gen.nextInt
    (new FixedGaussianGenerator(gaussian, gen0), x)
  }

  /**
   * Generates a random long. All 64-bit long values are equally likely.
   */
  def nextLong: (Generator, Long) = {
    val (gen0, x) = gen.nextLong
    (new FixedGaussianGenerator(gaussian, gen0), x)
  }

  override def nextGaussian: (Generator, Double) = (gen, gaussian)
}

private final class MutableWrapper(var gen: Generator) extends mutable.Generator {
  def copy: mutable.Generator = new MutableWrapper(gen)

  def getSeedBytes(): Array[Byte] = gen.getSeedBytes

  def setSeedBytes(bytes: Array[Byte]): Unit = gen = gen.withSeedBytes(bytes)

  def nextInt(): Int = {
    val (gen0, x) = gen.nextInt
    gen = gen0
    x
  }

  def nextLong(): Long = {
    val (gen0, x) = gen.nextLong
    gen = gen0
    x
  }

  override def toImmutable: Generator = gen
}

trait GeneratorCompanion[G <: Generator, @spec(Int, Long) S] {
  def fromBytes(bytes: Array[Byte]): G
  def fromSeed(seed: S): G
}
