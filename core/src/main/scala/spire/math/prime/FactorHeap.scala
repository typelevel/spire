package spire.math.prime

import SieveUtil._

/**
 * Simple heap implementation for storing Factors.
 * 
 * The heap can hold at most ~2B items, which means we can't store
 * prime factors larger than this.
 * 
 * The sieve implementation itself uses a cutoff anyway, so we would
 * only need to be able to hold more than ~2B factors if we wanted to
 * be able to find primes larger than the (2B * 2B)th prime number.
 * For these reasons, this particular limitation isn't expected to be
 * a problem.
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
