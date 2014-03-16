package spire.math.prime

import scala.reflect.ClassTag

import spire.syntax.cfor._

import Types._

/**
 * Represents a prime factor which we need to keep track of.
 *
 * The first field 'p' is the prime itself. The 'next' field is the
 * next multiple of the prime that we expect to see.
 * 
 * We use a slightly non-standard compare() function so that the
 * factor with the smallest 'next' field will be the largest.
 */
case class Factor(p: N, var next: N) extends Ordered[Factor] {
  def compare(that: Factor): Int = -(this.next compare that.next)
}


/**
 * 
 */
class FactorHeap {
  var arr: Array[Factor] = new Array[Factor](8)
  var len: Int = 0

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
