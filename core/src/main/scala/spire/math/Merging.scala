/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math
import spire.algebra.Order

/**
 * Interface for a merging strategy object.
 */
trait Merge extends Any {
  def merge[@sp A: Order: ClassTag](a: Array[A], b: Array[A]): Array[A]
}

/**
 * Abstract class that can be used to implement custom binary merges with e.g. special collision behavior or an ordering
 * that is not defined via an Order[T] typeclass
 */
abstract class BinaryMerge {

  final private[this] def binarySearchB(ai: Int, b0: Int, b1: Int): Int = {

    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if (low <= high) {
        val mid = (low + high) >>> 1
        val c = compare(ai, mid)
        if (c > 0)
          binarySearch0(mid + 1, high)
        else if (c < 0)
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(b0, b1 - 1)
  }

  /**
   * Compare element ai of the first sequence with element bi of the second sequence
   * @param ai
   *   an index into the first sequence
   * @param bi
   *   an index into the second sequence
   * @return
   * -1 if a(ai) &lt; b(bi), 0 if a(ai) == b(bi), 1 if a(ai) &gt; b(bi)
   */
  def compare(ai: Int, bi: Int): Int

  /**
   * Called when elements a(ai) and b(bi) are equal according to compare
   * @param ai
   * @param bi
   */
  def collision(ai: Int, bi: Int): Unit

  /**
   * Called for a subsequence of elements of a that are not overlapping any element of b
   */
  def fromA(a0: Int, a1: Int, bi: Int): Unit

  /**
   * Called for a subsequence of elements of b that are not overlapping any element of a
   */
  def fromB(ai: Int, b0: Int, b1: Int): Unit

  def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
    if (a0 == a1) {
      if (b0 != b1)
        fromB(a0, b0, b1)
    } else if (b0 == b1) {
      fromA(a0, a1, b0)
    } else {
      val am = (a0 + a1) / 2
      val res = binarySearchB(am, b0, b1)
      if (res >= 0) {
        // same elements
        val bm = res
        // merge everything below a(am) with everything below the found element
        merge0(a0, am, b0, bm)
        // add the elements a(am) and b(bm)
        collision(am, bm)
        // merge everything above a(am) with everything above the found element
        merge0(am + 1, a1, bm + 1, b1)
      } else {
        val bm = -res - 1
        // merge everything below a(am) with everything below the found insertion point
        merge0(a0, am, b0, bm)
        // add a(am)
        fromA(am, am + 1, bm)
        // everything above a(am) with everything above the found insertion point
        merge0(am + 1, a1, bm, b1)
      }
    }
  }
}

/**
 * Merge that uses binary search to reduce the number of comparisons
 *
 * This can be orders of magnitude quicker than a linear merge for types that have a relatively expensive comparison
 * operation (e.g. Rational, BigInt, tuples) and will not be much slower than linear merge even in the worst case for
 * types that have a very fast comparison (e.g. Int)
 */
object BinaryMerge extends Merge {

  def merge[@sp T: Order: ClassTag](a: Array[T], b: Array[T]): Array[T] = {
    new ArrayBinaryMerge(a, b).result
  }

  /*
  private[this] def resize[T:ClassTag](x:Array[T], n: Int): Array[T] = {
    if (n == x.length)
      x
    else {
      val t = Array.ofDim[T](n)
      System.arraycopy(x, 0, t, 0, n)
      t
    }
  }*/

  private class ArrayBinaryMerge[@specialized T](a: Array[T], b: Array[T])(implicit o: Order[T], c: ClassTag[T])
      extends BinaryMerge {

    def compare(ai: Int, bi: Int): Int = o.compare(a(ai), b(bi))

    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int): Unit = {
      r(ri) = a(ai)
      ri += 1
      r(ri) = b(bi)
      ri += 1
    }

    val r = Array.ofDim[T](a.length + b.length)
    var ri = 0
    merge0(0, a.length, 0, b.length)

    def result: Array[T] = r
  }
}

/**
 * Simple linear merge
 */
object LinearMerge extends Merge {

  def merge[@sp T: Order: ClassTag](a: Array[T], b: Array[T]): Array[T] = {
    val o = implicitly[Order[T]]
    val r = Array.ofDim[T](a.length + b.length)
    var ri = 0
    var ai = 0
    var bi = 0
    while (ai < a.length && bi < b.length) {
      val c = o.compare(a(ai), b(bi))
      if (c < 0) {
        r(ri) = a(ai)
        ri += 1
        ai += 1
      } else if (c > 0) {
        r(ri) = b(bi)
        ri += 1
        bi += 1
      } else {
        r(ri) = a(ai)
        ri += 1
        r(ri) = b(bi)
        ri += 1
        ai += 1
        bi += 1
      }
    }
    while (ai < a.length) {
      r(ri) = a(ai)
      ri += 1
      ai += 1
    }
    while (bi < b.length) {
      r(ri) = b(bi)
      ri += 1
      bi += 1
    }
    r
  }
}
