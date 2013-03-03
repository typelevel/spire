package spire.math

import scala.reflect.ClassTag
import scala.{specialized => spec}
import scala.annotation.tailrec

/**
 *  Interface for a sorting strategy object.
 */
trait Sort {
  def sort[@spec A:Order:ClassTag](data:Array[A]): Unit
}

/**
 * Simple implementation of insertion sort.
 *
 * Works for small arrays but due to O(n^2) complexity is not generally good.
 */
object InsertionSort extends Sort {
  final def sort[@spec A:Order:ClassTag](data:Array[A]) =
    sort(data, 0, data.length)

  final def sort[@spec A](data:Array[A], start:Int, end:Int)
    (implicit o:Order[A], ct:ClassTag[A]) {

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
 * array of the same size as the input. It uses InsertionSort for sorting very
 * small arrays.
 */
object MergeSort extends Sort {
  @inline final def startWidth = 8
  @inline final def startStep = 16

  final def sort[@spec A:Order:ClassTag](data:Array[A]) {
    val len = data.length

    if (len <= startStep) return InsertionSort.sort(data)

    var buf1:Array[A] = data
    var buf2:Array[A] = new Array[A](len)
    var tmp:Array[A] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) { InsertionSort.sort(data, i, i + startWidth); i += startWidth }
    if (i < len) InsertionSort.sort(data, i, len)
    var width = startWidth
    var step = startStep
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
 * In-place quicksort implementation. It is not stable, but does not allocate
 * extra space (other than stack). Like MergeSort, it uses InsertionSort for
 * sorting very small arrays.
 */
object QuickSort {
  @inline final def limit = 16

  final def sort[@spec A:Order:ClassTag](data:Array[A]) = qsort(data, 0, data.length - 1)

  final def qsort[@spec A]
    (data:Array[A], left: Int, right: Int)
    (implicit o:Order[A], ct:ClassTag[A]) {

    if (right - left < limit) return InsertionSort.sort(data, left, right + 1)

    val pivot = left + (right - left) / 2
    val next = partition(data, left, right, pivot)
    qsort(data, left, next - 1)
    qsort(data, next + 1, right)
  }

  final def partition[@spec A]
    (data:Array[A], left:Int, right:Int, pivot:Int)
    (implicit o:Order[A], ct:ClassTag[A]): Int = {

    val value = data(pivot)

    //swap(pivot, right)
    var tmp = data(pivot); data(pivot) = data(right); data(right) = tmp

    var store = left
    var i = left
    while (i < right) {
      if (o.lt(data(i), value)) {
        //swap(i, store)
        tmp = data(i); data(i) = data(store); data(store) = tmp
        store += 1
      }
      i += 1
    }
    //swap(store, right)
    tmp = data(store); data(store) = data(right); data(right) = tmp
    store
  }
}

// TODO: it would be nice to try implementing some hybrid sorts, for instance
// Tim Peters' sort algorithm.

/**
 * Object providing in-place sorting capability for arrays.
 *
 * Sorting.sort() uses quickSort() by default (in-place, not stable, generally
 * fastest but might hit bad cases where it's O(n^2)). Also provides
 * mergeSort() (in-place, stable, uses extra memory, still pretty fast) and
 * insertionSort(), which is slow except for small arrays.
 */
object Sorting {
  final def sort[@spec A:Order:ClassTag](data:Array[A]) = QuickSort.sort(data)

  final def insertionSort[@spec A:Order:ClassTag](data:Array[A]) = InsertionSort.sort(data)
  final def mergeSort[@spec A:Order:ClassTag](data:Array[A]) = MergeSort.sort(data)
  final def quickSort[@spec K:Order:ClassTag](data:Array[K]) = QuickSort.sort(data)
}
