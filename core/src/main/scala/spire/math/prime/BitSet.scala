package spire.math.prime

import spire.syntax.cfor._

/**
 * Fast BitSet implementation.
 *
 * This bitset is just intended to be a little bit faster than
 * Scala's, and to support accessing its internals, which we do in
 * some cases.
 * 
 * The max length (~2B) is a current limit to how big individual sieve
 * segments can get. Until our sieving is more efficient, we don't
 * want segments that big anyway, so this is OK.
 */
object BitSet {
  def alloc(nbuckets: Int): BitSet = {
    val n: Int = (nbuckets >> 5)
    val arr = new Array[Int](n)
    new BitSet(nbuckets, arr)
  }
}

case class BitSet(len: Int, arr: Array[Int]) {
  def length: Int = len

  def +=(n: Int) {
    val q = n >>> 5
    arr(q) = arr(q) | (1 << (n & 31))
  }

  def -=(n: Int) {
    val q = n >>> 5
    arr(q) = arr(q) & ~(1 << (n & 31))
  }

  def update(n: Int, b: Boolean): Unit =
    if (b) this += n else this -= n

  def apply(n: Int): Boolean = {
    val q = n >>> 5
    ((arr(q) >>> (n & 31)) & 1) == 1
  }

  def clear(): Unit =
    cfor(0)(_ < arr.length, _ + 1)(arr(_) = 0)
}
