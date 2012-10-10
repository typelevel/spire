package spire.math

import scala.reflect.ClassTag
import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.math.min

//import Implicits._

/**
 *  Interface for a sorting strategy object.
 */
trait Sort {
  def sort[@spec A:Order:ClassTag](data:Array[A]): Unit
}

object InsertionSort extends Sort {
  final def sort[@spec A:Order:ClassTag](data:Array[A]) = sort(data, 0, data.length)

  final def sort[@spec A](data:Array[A], start:Int, end:Int)
  (implicit o:Order[A], ct:ClassTag[A]) = {
    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/**
 * In-place merge sort implementation. This sort is stable but does mutate
 * the given array. It is an in-place sort but it does allocate a temporary
 * array of the same size as the input.
 *
 * This sort is faster than quickSort, but must allocate extra space.
 */
object MergeSort extends Sort {
  final def sort[@spec A:Order:ClassTag](data:Array[A]) = {
    val len = data.length

    var buf1:Array[A] = data
    var buf2:Array[A] = Array.ofDim[A](len)
    var tmp:Array[A] = null

    //var width = 1
    //var step = 2
    var i = 0
    var limit = len - 16
    while (i < limit) { InsertionSort.sort(data, i, i + 16); i += 16 }
    if (i < len) InsertionSort.sort(data, i, len)
    var width = 16
    var step = 32
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        merge(buf1, buf2, i, i + width, i + step); i += step
      }
      while (i < len) {
        merge(buf1, buf2, i, min(i + width, len), len); i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp
    
      width *= 2
      step *= 2
    }

    if (buf1 != data) System.arraycopy(buf1, 0, data, 0, len)
  }

  // TODO: making this private breaks specialization, but we'd like to hide it
  // somehow. 2.11 maybe?
  /**
   * Helper method for mergeSort, used to do a single "merge" between two
   * sections of the input array. The start, mid and end parameters denote the
   * left and right ranges of the input to merge, as well as the area of the
   * ouput to write to.
   */
  @inline final def merge[@spec A]
    (in:Array[A], out:Array[A], start:Int, mid:Int, end:Int)
    (implicit o:Order[A]) {
          
    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteqv(in(ii), in(jj)))) {
        out(kk) = in(ii); ii += 1
      } else {
        out(kk) = in(jj); jj += 1
      }
      kk += 1
    }
  }
}

/**
 * This is a specialized version of Scala's built-in quicksort
 * implementation. It is not a stable sort, and mutates its input. It does
 * not allocate extra space.
 */
object QuickSort {
  final def sort[@spec K](x:Array[K])(implicit o:Order[K], ct:ClassTag[K]) {
    sort2(x:Array[K], x(0), 0, x.length)
  }

  def sort2[@spec K](x: Array[K], k:K, off: Int, len: Int)
    (implicit o:Order[K], ct:ClassTag[K]) {

    def swap(k:K, a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
  
    def vecswap(k:K, _a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(k, a, b)
        i += 1
        a += 1
        b += 1
      }
    }
  
    def med3(k:K, a: Int, b: Int, c: Int) = {
      if (o.lt(x(a), x(b))) {
        if (o.lt(x(b), x(c))) b else if (o.lt(x(a), x(c))) c else a
      } else {
        if (o.lt(x(b), x(c))) b else if (o.lt(x(a), x(c))) c else a
      }
    }

    // Insertion sort on smallest arrays
    if (len < 7) {
      var i = off
      while (i < len + off) {
        var j = i
        while (j > off && o.gt(x(j-1), x(j))) {
          swap(k, j, j-1)
          j -= 1
        }
        i += 1
      }
    } else {
      // Choose a partition element, v
      var m = off + (len >> 1)        // Small arrays, middle element
      if (len > 7) {
        var l = off
        var n = off + len - 1
        if (len > 40) {        // Big arrays, pseudomedian of 9
          val s = len / 8
          l = med3(k, l, l+s, l+2*s)
          m = med3(k, m-s, m, m+s)
          n = med3(k, n-2*s, n-s, n)
        }
        m = med3(k, l, m, n) // Mid-size, med of 3
      }
      val v = x(m)

      // Establish Invariant: v* (<v)* (>v)* v*
      var a = off
      var b = a
      var c = off + len - 1
      var d = c
      var done = false
      while (!done) {
        while (b <= c && o.lteqv(x(b), v)) {
          if (o.eqv(x(b), v)) {
            swap(k, a, b)
            a += 1
          }
          b += 1
        }
        while (c >= b && o.gteqv(x(c), v)) {
          if (o.eqv(x(c), v)) {
            swap(k, c, d)
            d -= 1
          }
          c -= 1
        }
        if (b > c) {
          done = true
        } else {
          swap(k, b, c)
          c -= 1
          b += 1
        }
      }

      // Swap partition elements back to middle
      val n = off + len
      var s = math.min(a-off, b-a)
      vecswap(k, off, b-s, s)
      s = math.min(d-c, n-d-1)
      vecswap(k, b, n-s, s)

      // Recursively sort non-partition-elements
      s = b - a
      if (s > 1)
        sort2(x, k, off, s)(o, ct)
      s = d - c
      if (s > 1)
        sort2(x, k, n-s, s)(o, ct)
    }
  }
}

// TODO: would be nice to try implementing e.g. Tim Peters' sort algorithm.

/**
 * Both sorts are roughly the same speed right now. Merge sort is stable but
 * uses extra memory. Quicksort is a bit faster but is O(n^2) in worst case.
 */
object Sorting {
  final def insertionSort[@spec A:Order:ClassTag](data:Array[A]) = InsertionSort.sort(data)
  final def mergeSort[@spec A:Order:ClassTag](data:Array[A]) = MergeSort.sort(data)
  final def quickSort[@spec K:Order:ClassTag](data:Array[K]) = QuickSort.sort(data)
}
