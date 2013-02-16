package spire.random

import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

trait Generator {
  def copy: Generator

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
   * Generate an equally-distributed random value.
   *
   * Requires a Next[A]. This usually means the type has a fixed resolution.
   */
  def iterator[A](implicit next: Next[A]): Iterator[A] =
    new NextIterator(next, this)

  /**
   * Generates a random integer using n bits of state (0 <= n <= 32).
   */
  def nextBits(n: Int): Int = nextInt() >>> (32 - n)

  /**
   * Generates a random int between 0 (inclusive) and n (exclusive). All values
   * should be equally likely.
   */
  def nextInt(n: Int): Int = {
    @tailrec def loop(b: Int): Int = {
      val v = b % n
      if (b - v + (n - 1) < 0) loop(nextInt() >>> 1) else v
    }

    if (n < 1)
      sys.error("integer input must be positive %d" format n)
    else if ((n & -n) == n)
      ((n * ((nextInt() >>> 1).toLong)) >>> 31).toInt
    else
      loop(nextInt() >>> 1)
  }

  def roll(n: Int) = (n * nextDouble).toInt

  /**
   * Generates a random Boolean.
   */
  def nextBoolean(): Boolean =
    (nextInt() & 1) != 0

  /**
   * Generates a random float in the interval 0.0 (inclusive) to 1.0 (exclusive).
   */
  def nextFloat(): Float =
    (nextInt() >>> 8) * 5.9604645e-8f

  /**
   * Generates a random double in the interval [from, until).
   */
  def nextFloat(from: Float, until: Float): Float = {
    if (until <= from)
      throw new IllegalArgumentException("%s is not less than %s" format (from, until))
    from + (until - from) * nextFloat()
  }

  /**
   * Generates a random double in the interval [0.0, 1.0).
   */
  def nextDouble(): Double =
    (((nextInt() >>> 6).toLong << 27) + (nextInt() >>> 5)) * 1.1102230246251565e-16

  /**
   * Generates a random double in the interval [from, until).
   */
  def nextDouble(from: Double, until: Double): Double = {
    if (until <= from)
      throw new IllegalArgumentException("%s is not less than %s" format (from, until))
    from + (until - from) * nextDouble()
  }

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

  def generateArray[A: Next: ClassTag](n: Int): Array[A] = {
    val arr = new Array[A](n)
    fillArray(arr)
    arr
  }

  def fillArray[A: Next](arr: Array[A]) {
    var i = 0
    val len = arr.length
    while (i < len) {
      arr(i) = next[A]
      i += 1
    }
  }
}

trait IntGenerator extends Generator {
  def nextLong(): Long =
    ((nextInt & 0xffffffffL) << 32) | (nextInt & 0xffffffffL)
}

trait LongGenerator extends Generator {
  def nextInt(): Int = (nextLong >>> 32).toInt

  override def nextDouble(): Double =
    (nextLong >>> 11) * 1.1102230246251565e-16
}
