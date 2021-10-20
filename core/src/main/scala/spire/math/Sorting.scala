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
 * Interface for a sorting strategy object.
 */
trait Sort extends Any {
  def sort[@sp A: Order: ClassTag](data: Array[A]): Unit
}

/**
 * An implementation of insertion sort.
 *
 * Works well for small arrays but due to quadratic complexity is not generally optimal.
 */
object InsertionSort extends Sort {

  /**
   * Sorts `data` in place using insertion sort.
   *
   * @param data
   *   the array to be sorted
   * @tparam A
   *   a member of the type class `Order`
   */
  final def sort[@sp A: Order: ClassTag](data: Array[A]): Unit =
    sort(data, 0, data.length)

  /**
   * Uses insertion sort on `data` to sort the entries from the index `start` up to, but not including, the index `end`.
   * Operates in place.
   *
   * @param data
   *   the array to be sorted
   * @param start
   *   the index of the first element, inclusive, to be sorted
   * @param end
   *   the index of the last element, exclusive, to be sorted
   * @tparam A
   *   a member of the type class `Order`
   */
  final def sort[@sp A: ClassTag: Order](data: Array[A], start: Int, end: Int): Unit = {
    require(start <= end && start >= 0 && end <= data.length)
    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && Order[A].gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/**
 * In-place merge sort implementation. This sort is stable but does mutate the given array. It is an in-place sort but
 * it does allocate a temporary array of the same size as the input. It uses InsertionSort for sorting very small
 * arrays.
 */
object MergeSort extends Sort {
  @inline final def startWidth: Int = 8
  @inline final def startStep: Int = 16

  /**
   * Uses merge sort to sort the array `data` in place.
   *
   * If the size of the input array does not exceed the threshold `startStep`, uses insertion sort instead.
   *
   * @param data
   *   the array to be sorted
   * @tparam A
   *   a member of the type class `Order`
   */
  final def sort[@sp A: Order: ClassTag](data: Array[A]): Unit = {
    val len = data.length

    if (len <= startStep) {
      InsertionSort.sort(data)
      return
    }

    var buf1: Array[A] = data
    var buf2: Array[A] = new Array[A](len)
    var tmp: Array[A] = null

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

    if (!buf1.eq(data)) System.arraycopy(buf1, 0, data, 0, len)
  }

  /**
   * Helper method for mergeSort, used to do a single "merge" between two sections of the input array, and write the
   * result to the output array.
   *
   * The first input section starts at `start` (inclusive) and ends at `mid` (exclusive). The second input section
   * starts at `mid` (inclusive) and ends at `end` (exclusive).
   *
   * Writing to the output begins at `start` (inclusive).
   *
   * @param in
   *   the input array
   * @param out
   *   the output array
   * @param start
   *   the start of the first input section (inclusive) as well as the start of the merged output
   * @param mid
   *   the end of the first input section (exclusive) and the beginning of the second input section (inclusive)
   * @param end
   *   the end of the second input section (exclusive)
   * @tparam A
   *   a member of the type class `Order`
   */
  @inline final def merge[@sp A](in: Array[A], out: Array[A], start: Int, mid: Int, end: Int)(implicit
    o: Order[A]
  ): Unit = {
    require(start >= 0 && start <= mid && mid <= end && end <= in.length && end <= out.length)
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
 * In-place quicksort implementation. It is not stable, but does not allocate extra space (other than stack). Like
 * MergeSort, it uses InsertionSort for sorting very small arrays.
 */
object QuickSort {
  @inline final def limit: Int = 16

  /**
   * Uses quicksort on `data` to sort the entries. Operates in place.
   *
   * If the size of the input array is less than the threshold `limit`, uses insertion sort instead.
   *
   * @param data
   *   the array to be sorted
   * @tparam A
   *   a member of the type class `Order`
   */
  final def sort[@sp A: Order: ClassTag](data: Array[A]): Unit = qsort(data, 0, data.length)

  /**
   * Uses quicksort on `data` to sort the entries from the index `start` up to, but not including, the index `end`.
   * Operates in place.
   *
   * If the size of the segment to be sorted is less than the threshold `limit` uses insertion sort instead.
   *
   * @param data
   *   the array to be sorted
   * @param start
   *   the index from which to start sorting (inclusive)
   * @param end
   *   the index at which to stop sorting (exclusive)
   * @tparam A
   *   a member of the type class `Order`
   */
  final def qsort[@sp A](data: Array[A], start: Int, end: Int)(implicit o: Order[A], ct: ClassTag[A]): Unit = {
    require(start >= 0 && end <= data.length)
    if (end - start < limit) {
      InsertionSort.sort(data, start, end)
      return
    }

    val pivotIndex = start + (end - start) / 2
    val nextPivotIndex = partition(data, start, end, pivotIndex)
    qsort(data, start, nextPivotIndex)
    qsort(data, nextPivotIndex + 1, end)
  }

  /**
   * Helper method for the quick sort implementation. Partitions the segment of the array `data` from `start` to `end`
   * according to the value at the given `pivotIndex`. Values in the segment less than the pivot value will end up to
   * the left of the pivot value, and values greater on the right. Operates in place.
   *
   * @param data
   *   the array to be partitioned
   * @param start
   *   the left endpoint (inclusive) of the interval to be partitioned
   * @param end
   *   the right endpoint (exclusive) of the interval to be partitioned
   * @param pivotIndex
   *   the index of the current pivot
   * @tparam A
   *   a member of the type class Order
   * @return
   *   the next pivot value
   */
  final def partition[@sp A: ClassTag: Order](data: Array[A], start: Int, end: Int, pivotIndex: Int): Int = {
    require(start >= 0 && pivotIndex >= start && end > pivotIndex && end <= data.length)
    val pivotValue = data(pivotIndex)

    data(pivotIndex) = data(end - 1)

    var temp = pivotValue
    var store = start
    var i = start
    while (i < end - 1) {
      if (Order[A].lt(data(i), pivotValue)) {
        //swap(i, store)
        temp = data(i); data(i) = data(store); data(store) = temp
        store += 1
      }
      i += 1
    }

    data(end - 1) = data(store)
    data(store) = pivotValue
    store
  }
}

// TODO: it would be nice to try implementing some hybrid sorts, for instance
// Tim Peters' sort algorithm.

/**
 * Object providing in-place sorting capability for arrays.
 *
 * Sorting.sort() uses quickSort() by default (in-place, not stable, generally fastest but might hit bad cases where it
 * is quadratic. Also provides mergeSort() (in-place, stable, uses extra memory, still pretty fast) and insertionSort(),
 * which is slow except for small arrays.
 */
object Sorting {

  /**
   * Delegates to [[spire.math.QuickSort.sort]]
   */
  final def sort[@sp A: Order: ClassTag](data: Array[A]): Unit = QuickSort.sort(data)

  /**
   * Delegates to [[spire.math.InsertionSort.sort]]
   */
  final def insertionSort[@sp A: Order: ClassTag](data: Array[A]): Unit = InsertionSort.sort(data)

  /**
   * Delegates to [[spire.math.MergeSort.sort]]
   */
  final def mergeSort[@sp A: Order: ClassTag](data: Array[A]): Unit = MergeSort.sort(data)

  /**
   * Delegates to [[spire.math.QuickSort.sort]]
   */
  final def quickSort[@sp A: Order: ClassTag](data: Array[A]): Unit = QuickSort.sort(data)
}
