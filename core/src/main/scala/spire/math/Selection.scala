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

trait Select extends Any {
  def select[@sp A: Order: ClassTag](data: Array[A], k: Int): Unit
}

/**
 * Given a function for finding approximate medians, this will create an exact median finder.
 */
trait SelectLike extends Any with Select {

  def approxMedian[@sp A: Order](data: Array[A], left: Int, right: Int, stride: Int): A

  /**
   * Puts the k-th element of data, according to some Order, in the k-th position. All values before k are less than or
   * equal to data(k) and all values above k are greater than or equal to data(k).
   *
   * This is an in-place algorithm and is not stable and it WILL mess up the order of equal elements.
   */
  final def select[@sp A: Order: ClassTag](data: Array[A], k: Int): Unit = {
    select(data, 0, data.length, 1, k)
  }

  // Copy of InsertSort.sort, but with a stride.
  final def sort[@sp A](data: Array[A], left: Int, right: Int, stride: Int)(implicit o: Order[A]): Unit = {
    var i = left
    while (i < right) {
      val item = data(i)
      var hole = i
      while (hole > left && o.gt(data(hole - stride), item)) {
        data(hole) = data(hole - stride)
        hole -= stride
      }
      data(hole) = item
      i += stride
    }
  }

  @tailrec
  final protected def select[@sp A: Order](data: Array[A], left: Int, right: Int, stride: Int, k: Int): Unit = {
    val length = (right - left + stride - 1) / stride
    if (length < 10) {
      sort(data, left, right, stride)

    } else {
      val c = partition(data, left, right, stride)(approxMedian(data, left, right, stride))
      val span = equalSpan(data, c, stride)

      if (c <= k && k < (c + span)) {
        // Spin.
      } else if (k < c) {
        select(data, left, c, stride, k)
      } else {
        val newLeft = c + span * stride
        select(data, newLeft, right, stride, k)
      }
    }
  }

  final def equalSpan[@sp A](data: Array[A], offset: Int, stride: Int)(implicit o: Order[A]): Int = {
    val m = data(offset)
    var i = offset + stride
    var len = 1
    while (i < data.length && o.eqv(m, data(i))) {
      i += stride
      len += 1
    }
    len
  }

  final def partition[@sp A](data: Array[A], left: Int, right: Int, stride: Int)(m: A)(implicit o: Order[A]): Int = {
    var i = left // Iterator.
    var j = left // Pointer to first element > m.
    var k = left // Pointer to end of equal elements.
    var t = m

    while (i < right) {
      val cmp = o.compare(data(i), m)
      if (cmp < 0) {
        t = data(i); data(i) = data(j); data(j) = t
        j += stride
      } else if (cmp == 0) {
        t = data(i); data(i) = data(j); data(j) = data(k); data(k) = t
        k += stride
        j += stride
      }

      i += stride
    }

    while (k > left) {
      j -= stride
      k -= stride
      t = data(j)
      data(j) = data(k)
      data(k) = t
    }

    j
  }
}

trait HighBranchingMedianOf5 {

  // Benchmarks show that this is slightly faster than the version above.

  // scalastyle:off method.length
  final def mo5[@sp A](data: Array[A], offset: Int, stride: Int)(implicit o: Order[A]): Unit = {
    val ai1 = data(offset)
    val ai2 = data(offset + stride)
    val ai3 = data(offset + 2 * stride)
    val ai4 = data(offset + 3 * stride)
    val ai5 = data(offset + 4 * stride)

    val i = if (o.lt(ai1, ai2)) { // i1 < i2
      if (o.lt(ai3, ai4)) { // i1 < i2, i3 < i4
        if (o.lt(ai2, ai4)) { // Drop i4
          if (o.lt(ai3, ai5)) { // i1 < i2, i3 < i5
            if (o.lt(ai2, ai5)) { // Drop i5
              if (o.lt(ai2, ai3)) offset + 2 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai1, ai5)) offset + 4 * stride else offset + 0 * stride
            }
          } else { // i1 < i2, i5 < i3
            if (o.lt(ai2, ai3)) { // Drop i3
              if (o.lt(ai2, ai5)) offset + 4 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai1, ai3)) offset + 2 * stride else offset + 0 * stride
            }
          }
        } else { // Drop i2
          if (o.lt(ai1, ai5)) { // i1 < i5, i3 < i4
            if (o.lt(ai5, ai4)) { // Drop i4
              if (o.lt(ai5, ai3)) offset + 2 * stride else offset + 4 * stride
            } else { // Drop i5
              if (o.lt(ai1, ai4)) offset + 3 * stride else offset + 0 * stride
            }
          } else { // i5 < i1, i3 < i4
            if (o.lt(ai1, ai4)) { // Drop i4
              if (o.lt(ai1, ai3)) offset + 2 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai5, ai4)) offset + 3 * stride else offset + 4 * stride
            }
          }
        }
      } else { // i1 < i2, i4 < i3
        if (o.lt(ai2, ai3)) { // Drop i3
          if (o.lt(ai4, ai5)) { // i1 < i2, i4 < i5
            if (o.lt(ai2, ai5)) { // Drop i5
              if (o.lt(ai2, ai4)) offset + 3 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai1, ai5)) offset + 4 * stride else offset + 0 * stride
            }
          } else { // i1 < i2, i5 < i4
            if (o.lt(ai2, ai4)) { // Drop i4
              if (o.lt(ai2, ai5)) offset + 4 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai1, ai4)) offset + 3 * stride else offset + 0 * stride
            }
          }
        } else { // Drop i2
          if (o.lt(ai1, ai5)) { // i1 < i5, i4 < i3
            if (o.lt(ai5, ai3)) { // Drop i3
              if (o.lt(ai5, ai4)) offset + 3 * stride else offset + 4 * stride
            } else { // Drop i5
              if (o.lt(ai1, ai3)) offset + 2 * stride else offset + 0 * stride
            }
          } else { // i5 < i1, i4 < i3
            if (o.lt(ai1, ai3)) { // Drop i3
              if (o.lt(ai1, ai4)) offset + 3 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai5, ai3)) offset + 2 * stride else offset + 4 * stride
            }
          }
        }
      }
    } else { // i2 < i1
      if (o.lt(ai3, ai4)) { // i2 < i1, i3 < i4
        if (o.lt(ai1, ai4)) { // Drop i4
          if (o.lt(ai3, ai5)) { // i2 < i1, i3 < i5
            if (o.lt(ai1, ai5)) { // Drop i5
              if (o.lt(ai1, ai3)) offset + 2 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai2, ai5)) offset + 4 * stride else offset + 1 * stride
            }
          } else { // i2 < i1, i5 < i3
            if (o.lt(ai1, ai3)) { // Drop i3
              if (o.lt(ai1, ai5)) offset + 4 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai2, ai3)) offset + 2 * stride else offset + 1 * stride
            }
          }
        } else { // Drop i1
          if (o.lt(ai2, ai5)) { // i2 < i5, i3 < i4
            if (o.lt(ai5, ai4)) { // Drop i4
              if (o.lt(ai5, ai3)) offset + 2 * stride else offset + 4 * stride
            } else { // Drop i5
              if (o.lt(ai2, ai4)) offset + 3 * stride else offset + 1 * stride
            }
          } else { // i5 < i2, i3 < i4
            if (o.lt(ai2, ai4)) { // Drop i4
              if (o.lt(ai2, ai3)) offset + 2 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai5, ai4)) offset + 3 * stride else offset + 4 * stride
            }
          }
        }
      } else { // i2 < i1, i4 < i3
        if (o.lt(ai1, ai3)) { // Drop i3
          if (o.lt(ai4, ai5)) { // i2 < i1, i4 < i5
            if (o.lt(ai1, ai5)) { // Drop i5
              if (o.lt(ai1, ai4)) offset + 3 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai2, ai5)) offset + 4 * stride else offset + 1 * stride
            }
          } else { // i2 < i1, i5 < i4
            if (o.lt(ai1, ai4)) { // Drop i4
              if (o.lt(ai1, ai5)) offset + 4 * stride else offset + 0 * stride
            } else { // Drop i1
              if (o.lt(ai2, ai4)) offset + 3 * stride else offset + 1 * stride
            }
          }
        } else { // Drop i1
          if (o.lt(ai2, ai5)) { // i2 < i5, i4 < i3
            if (o.lt(ai5, ai3)) { // Drop i3
              if (o.lt(ai5, ai4)) offset + 3 * stride else offset + 4 * stride
            } else { // Drop i5
              if (o.lt(ai2, ai3)) offset + 2 * stride else offset + 1 * stride
            }
          } else { // i5 < i2, i4 < i3
            if (o.lt(ai2, ai3)) { // Drop i3
              if (o.lt(ai2, ai4)) offset + 3 * stride else offset + 1 * stride
            } else { // Drop i2
              if (o.lt(ai5, ai3)) offset + 2 * stride else offset + 4 * stride
            }
          }
        }
      }
    }

    val m = data(i)
    data(i) = data(offset)
    data(offset) = m
  }
  // scalastyle:on method.length
}

object LinearSelect extends SelectLike with HighBranchingMedianOf5 {

  // We need to guarantee linear time complexity, so we have to get down and
  // actually find a pivot w/ a good constant fraction of the data points on
  // one side. This makes this quite a bit slower in the general case (though
  // not terribly so), but doesn't suffer from bad worst-case behaviour.

  final def approxMedian[@sp A: Order](data: Array[A], left: Int, right: Int, stride: Int): A = {
    var offset = left
    var last = left + 4 * stride
    val nextStride = 5 * stride

    while (last < right) {
      mo5(data, offset, stride)
      offset += nextStride
      last += nextStride
    }

    val length = (right - left + nextStride - 1) / nextStride
    val k = left + ((length - 1) / 2) * nextStride
    select(data, left, right, nextStride, k)
    data(k)
  }
}

object QuickSelect extends SelectLike with HighBranchingMedianOf5 {

  // For large arrays, the partitioning dominates the runtime. Choosing a good
  // pivot, quickly is essential. So, we have 3 cases, getting slightly smarter
  // about our pivot as the array grows.

  final def approxMedian[@sp A: Order](data: Array[A], left: Int, right: Int, stride: Int): A = {
    val length = (right - left + stride - 1) / stride

    if (length >= 5) {
      val p2stride = stride * (length / 5)

      if (length >= 125) {
        val p1stride = stride * (length / 25)
        mo5(data, left, p1stride)
        mo5(data, left + p2stride, p1stride)
        mo5(data, left + 2 * p2stride, p1stride)
        mo5(data, left + 3 * p2stride, p1stride)
        mo5(data, left + 4 * p2stride, p1stride)
      }

      mo5(data, left, p2stride)
    }

    data(left)
  }
}

object Selection {
  final def select[@sp A: Order: ClassTag](data: Array[A], k: Int): Unit =
    quickSelect(data, k)

  final def linearSelect[@sp A: Order: ClassTag](data: Array[A], k: Int): Unit =
    LinearSelect.select(data, k)

  final def quickSelect[@sp A: Order: ClassTag](data: Array[A], k: Int): Unit =
    QuickSelect.select(data, k)
}
