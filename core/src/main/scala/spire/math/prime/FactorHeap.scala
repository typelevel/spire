package spire.math.prime

import SieveUtil._

/**
 * Simple heap implementation for storing Factors.
 * 
 * The heap can hold at most ~2B items, which means we can't store
 * more than this many prime factors.
 * 
 * Note that "fast factors" don't end up in this heap, so the number
 * of primes we can sieve is actaully the max heap size + the number
 * of fast factors.
 * 
 * The sieve implementation itself uses a cutoff, so to test primality
 * of numbers <= K, we need to be able to store prime factors up to
 * sqrt(K) in our heap. Since our heap can hold ~2B prime factors,
 * this means the theoretical upper bound on our segmented sieve is
 * (~2Bth prime)^2.
 * 
 * In practice the sieve will slow down to the point of not being
 * useful far before we could reach this limit.
 */
class FactorHeap {

  private[this] var arr: Array[Factor] = new Array[Factor](8)
  private[this] var len: Int = 0

  override def toString: String =
    arr.filter(_ != null).map(_.next).mkString("FactorHeap(", ", ", ")")

  def isEmpty: Boolean = len == 0

  def nonEmpty: Boolean = len > 0

  def size: Int = len

  def resizeIfNecessary(): Unit =
    if (len >= arr.length) {
      val arr2 = new Array[Factor](arr.length * 2)
      System.arraycopy(arr, 0, arr2, 0, arr.length)
      arr = arr2
    }

  def +=(factor: Factor): Unit = {
    len += 1
    resizeIfNecessary()
    var i = len
    while (i > 1) {
      val j = i >>> 1
      val fj = arr(j)
      if (factor.next >= fj.next) { arr(i) = factor; return () }
      arr(i) = fj
      i = j
    }
    arr(i) = factor
  }

  def dequeue(): Factor = {
    if (len == 0) throw new NoSuchElementException("empty heap")
    val result = arr(1)
    val last = arr(len)
    len -= 1 // points to last valid slot

    var i = 1
    var j = 2
    while (len >= j) {
      if (j < len && arr(j).next > arr(j + 1).next) j += 1
      val cv = arr(j)
      if (last.next <= cv.next) { arr(i) = last; return result }
      arr(i) = cv
      i = j
      j *= 2
    }
    arr(i) = last
    result
  }
}
