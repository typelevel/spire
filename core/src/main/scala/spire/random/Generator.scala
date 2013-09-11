package spire.random

import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

abstract class Generator {
  protected var extra: Boolean = false
  protected var value: Double = 0.0

  // TODO: copy may not generate the same gaussians as the original.
  def copy: Generator

  def sync: SyncGenerator = new SyncGenerator(copy)

  def getSeedBytes(): Array[Byte]

  def setSeedBytes(bytes: Array[Byte]): Unit

  /**
   * Generate an equally-distributed random Int.
   */
  def nextInt(): Int

  /**
   * Generates a random long. All 64-bit long values are equally likely.
   */
  def nextLong(): Long

  /**
   * Generate a random value using a Dist[A] type class instance.
   *
   * Implicit Dist[A] instances are provided for the AnyVal types as well as
   * UByte through ULong. More complex Dist instances can be created from
   * these.
   */
  def next[A](implicit next: Dist[A]): A = next(this)

  /**
   * Generate an infinite iterator of random values using Dist[A].
   */
  def iterator[A](implicit next: Dist[A]): Iterator[A] =
    new DistIterator(next, this)

  /**
   * Generates a random integer using n bits of state (0 <= n <= 32).
   */
  def nextBits(n: Int): Int = nextInt() >>> (32 - n)

  /**
   * Generates a random int between 0 (inclusive) and n (exclusive).
   */
  def nextInt(n: Int): Int = {
    @tailrec def loop(b: Int): Int = {
      val v = b % n
      if (b - v + (n - 1) < 0) loop(nextInt() >>> 1) else v
    }

    if (n < 1)
      throw new IllegalArgumentException("argument must be positive %d" format n)
    else if ((n & -n) == n)
      ((n * ((nextInt() >>> 1).toLong)) >>> 31).toInt
    else
      loop(nextInt() >>> 1)
  }

  private final def retryCap(width: UInt): UInt = {
    // Simulate 1 << 32 long division.
    val q = UInt(Int.MinValue) / width
    val r = UInt(Int.MinValue) % width
    val n = (q << 1) + (r << 1) / width
    n * width
  }

  /**
   * Return an Int in [from, to].
   */
  def nextInt(from: Int, to: Int): Int = {
    val width = UInt(to - from + 1)
    if (width == UInt(0)) {
      nextInt()
    } else {
      val cap = if (width > UInt(Int.MinValue)) width else retryCap(width)
      if (cap == UInt(0)) {
        val x = UInt(nextInt())
        from + (x % width).signed
      } else {
        @tailrec def loop(): Int = {
          val x = UInt(nextInt())
          if (x <= cap) (x % width).signed + from else loop()
        }
        loop()
      }
    }
  }

  /**
   * Generates a random int between 0 (inclusive) and n (exclusive).
   */
  def nextLong(n: Long): Long = {
    @tailrec def loop(b: Long): Long = {
      val v = b % n
      if (b - v + (n - 1) < 0) loop(nextLong() >>> 1) else v
    }

    if (n < 1)
      throw new IllegalArgumentException("argument must be positive %d" format n)
    else if ((n & -n) == n)
      nextLong() & (n - 1)
    else
      loop(nextLong() >>> 1)
  }

  private final def retryCap(width: ULong): ULong = {
    // Simulate 1 << 32 long division.
    val q = ULong(Long.MinValue) / width
    val r = ULong(Long.MinValue) % width
    val n = (q << 1) + (r << 1) / width
    n * width
  }

  /**
   * Return an Long in [from, to].
   */
  def nextLong(from: Long, to: Long): Long = {
    val width = ULong(to - from + 1)
    if (width == ULong(0)) {
      nextLong()
    } else {
      val cap = if (width > ULong(Long.MinValue)) width else retryCap(width)
      if (cap == ULong(0)) {
        val x = ULong(nextLong())
        from + (x % width).signed
      } else {
        @tailrec def loop(): Long = {
          val x = ULong(nextLong())
          if (x <= cap) (x % width).signed + from else loop()
        }
        loop()
      }
    }
  }

  /**
   * Generates a random Boolean.
   */
  def nextBoolean(): Boolean = (nextInt() & 1) != 0

  /**
   * Generates a random float in [0.0, 1.0).
   */
  def nextFloat(): Float = (nextInt() >>> 8) * 5.9604645e-8f

  /**
   * Generates a random float in [0.0, n).
   */
  def nextFloat(n: Float): Float = nextFloat() * n

  /**
   * Generates a random float in [from, until).
   */
  def nextFloat(from: Float, until: Float): Float =
    from + (until - from) * nextFloat()

  /**
   * Generates a random double in [0.0, 1.0).
   */
  def nextDouble(): Double =
    (nextLong() >>> 11) * 1.1102230246251565e-16

  /**
   * Generates a random double in [0.0, n).
   */
  def nextDouble(n: Double): Double = nextDouble() * n

  /**
   * Generates a random double in [from, until).
   */
  def nextDouble(from: Double, until: Double): Double =
    from + (until - from) * nextDouble()

  /**
   * Generate an array of n random Longs.
   */
  def generateLongs(n: Int): Array[Long] = {
    val arr = new Array[Long](n)
    fillLongs(arr)
    arr
  }

  /**
   * Fill an array with random Longs.
   */
  def fillLongs(arr: Array[Long]) {
    var i = 0
    val len = arr.length
    while (i < len) {
      arr(i) = nextLong()
      i += 1
    }
  }

  /**
   * Generate an array of n random Ints.
   */
  def generateInts(n: Int): Array[Int] = {
    val arr = new Array[Int](n)
    fillInts(arr)
    arr
  }

  /**
   * Fill an array with random Ints.
   */
  def fillInts(arr: Array[Int]) {
    var i = 0
    val len = arr.length
    while (i < len) {
      arr(i) = nextInt()
      i += 1
    }
  }

  /**
   * Generate an array of n random Shorts.
   */
  def generateShorts(n: Int): Array[Short] = {
    val arr = new Array[Short](n)
    fillShorts(arr)
    arr
  }

  /**
   * Fill an array with random Shorts.
   */
  def fillShorts(arr: Array[Short]) {
    var i = 0
    val len = arr.length
    val llen = len & 0xfffffffe
    while (i < llen) {
      val n = nextInt()
      arr(i) = (n & 0xffff).toShort
      arr(i + 1) = ((n >>> 16) & 0xffff).toShort
      i += 2
    }

    if (len != llen) arr(i) = (nextInt() & 0xffff).toShort
  }

  /**
   * Generate an array of n random Bytes.
   */
  def generateBytes(n: Int): Array[Byte] = {
    val arr = new Array[Byte](n)
    fillBytes(arr)
    arr
  }

  /**
   * Fill an array with random Bytes.
   */
  def fillBytes(arr: Array[Byte]) {
    var i = 0
    val len = arr.length
    val llen = len & 0xfffffffc
    while (i < llen) {
      val n = nextInt()
      arr(i) = (n & 0xff).toByte
      arr(i + 1) = ((n >>> 8) & 0xff).toByte
      arr(i + 2) = ((n >>> 16) & 0xff).toByte
      arr(i + 3) = ((n >>> 24) & 0xff).toByte
      i += 4
    }

    if (i < len) {
      var n = nextInt()
      while (i < len) {
        arr(i) = (n & 0xff).toByte
        n = n >>> 8
        i += 1
      }
    }
  }

  /**
   * Generate an Array[A] using the given Dist[A] instance.
   */
  def generateArray[@spec A: Dist: ClassTag](n: Int): Array[A] = {
    val arr = new Array[A](n)
    fillArray(arr)
    arr
  }

  /**
   * Fill an Array[A] using the given Dist[A] instance.
   */
  def fillArray[@spec A: Dist](arr: Array[A]) {
    var i = 0
    val len = arr.length
    while (i < len) {
      arr(i) = next[A]
      i += 1
    }
  }

  def oneOf[A](as: A*): A = chooseFromSeq(as)(this)

  def chooseFromArray[@spec A](arr: Array[A])(implicit gen: Generator): A =
    arr(gen.nextInt(arr.length))

  def chooseFromSeq[A](seq: Seq[A])(implicit gen: Generator): A =
    seq(gen.nextInt(seq.length))

  def chooseFromIterable[A](as: Iterable[A])(implicit gen: Generator): A =
    as.iterator.drop(gen.nextInt(as.size)).next()

  def sampleFromArray[@spec A: ClassTag](as: Array[A], size: Int)(implicit gen: Generator): Array[A] = {
    val chosen: Array[A] = new Array[A](size)
    if (size < 1) {
      throw new IllegalArgumentException("illegal sample size (%d)" format size)
    } else if (size < as.length) {
      var i = 0
      while (i < as.length) {
        if (i < size) {
          chosen(i) = as(i)
        } else {
          val n: Int = gen.nextInt(i + 1)
          if (n < size) chosen(n) = as(i)
        }
        i += 1
      }

    } else if (size == as.length) {
      System.arraycopy(as, 0, chosen, 0, as.length)
      shuffle(chosen)
    } else {
      throw new IllegalArgumentException("sample size (%d) exceeds input size (%d)" format (size, as.length))
    }
    chosen
  }

  def sampleFromTraversable[@spec A: ClassTag](as: Traversable[A], size: Int)(implicit gen: Generator): Array[A] = {
    val chosen: Array[A] = new Array[A](size)
    var i: Int = 0
    as.foreach { a =>
      if (i < size) {
        chosen(i) = a
      } else {
        val n: Int = gen.nextInt(i + 1)
        if (n < size) chosen(n) = a
      }
      i += 1
    }
    if (i < size) {
      throw new IllegalArgumentException("sample size (%d) exceeds input size (%d)" format (size, i))
    }
    chosen
  }

  def shuffle[@spec A](as: Array[A])(implicit gen: Generator) {
    var i: Int = as.length - 1
    while (i > 0) {
      val n: Int = gen.nextInt(i)
      val tmp: A = as(i)
      as(i) = as(n)
      as(n) = tmp
      i -= 1
    }
  }

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

abstract class IntBasedGenerator extends Generator { self =>
  def nextLong(): Long =
    ((nextInt() & 0xffffffffL) << 32) | (nextInt() & 0xffffffffL)
}

abstract class LongBasedGenerator extends Generator { self =>
  def nextInt(): Int =
    (nextLong >>> 32).toInt
}

trait GeneratorCompanion[G, @spec(Int, Long) S] {
  def randomSeed(): S

  def fromBytes(bytes: Array[Byte]): G
  def fromSeed(seed: S): G
  def fromTime(time: Long = System.nanoTime): G

  final def apply(): G = fromTime()
  final def apply(seed: S): G = fromSeed(seed)
}

object Generator {
  implicit val rng = GlobalRng
}

object GlobalRng extends LongBasedGenerator {
  private val rng = Cmwc5.fromTime().sync

  override def sync = rng

  def copy: Generator = rng.copy

  def getSeedBytes: Array[Byte] = rng.getSeedBytes

  def setSeedBytes(bytes: Array[Byte]): Unit = rng.setSeedBytes(bytes)

  def nextLong(): Long = rng.nextLong()
}
