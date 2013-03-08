package spire.random

import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

abstract class Generator {
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
   * Generate a random value using a Next[A] type class instance.
   *
   * Implicit Next[A] instances are provided for the AnyVal types as well as
   * UByte through ULong. More complex Next instances can be created from
   * these.
   */
  def next[A](implicit next: Next[A]): A = next(this)

  /**
   * Generate an infinite iterator of random values using Next[A].
   */
  def iterator[A](implicit next: Next[A]): Iterator[A] =
    new NextIterator(next, this)

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

  /**
   * Return an Int in [from, to].
   */
  def nextInt(from: Int, to: Int): Int = from + nextInt(to - from + 1)

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
  def nextDouble(n: Float): Float = nextFloat() * n

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
   * Generate an Array[A] using the given Next[A] instance.
   */
  def generateArray[A: Next: ClassTag](n: Int): Array[A] = {
    val arr = new Array[A](n)
    fillArray(arr)
    arr
  }

  /**
   * Fill an Array[A] using the given Next[A] instance.
   */
  def fillArray[A: Next](arr: Array[A]) {
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
