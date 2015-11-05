package spire
package math

import scala.reflect.ClassTag

import spire.algebra.Order

trait Select extends Any {
  def select[@spec A: Order: ClassTag](data: Array[A], k: Int): Unit
}

/**
 * Given a function for finding approximate medians, this will create an exact
 * median finder.
 */
trait SelectLike extends Any with Select {

  def approxMedian[@spec A: Order](data: Array[A], left: Int, right: Int, stride: Int): A

  /**
   * Puts the k-th element of data, according to some Order, in the k-th
   * position. All values before k are less than or equal to data(k) and all
   * values above k are greater than or equal to data(k).
   *
   * This is an in-place algorithm and is not stable and it WILL mess up the
   * order of equal elements.
   */
  final def select[@spec A: Order: ClassTag](data: Array[A], k: Int): Unit = {
    select(data, 0, data.length, 1, k)
  }

  // Copy of InsertSort.sort, but with a stride.
  final def sort[@spec A](data: Array[A], left: Int, right: Int, stride: Int)(implicit o: Order[A]): Unit = {
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
  protected final def select[@spec A: Order](data: Array[A], left: Int, right: Int, stride: Int, k: Int): Unit = {
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

  final def equalSpan[@spec A](data: Array[A], offset: Int, stride: Int)(implicit o: Order[A]): Int = {
    val m = data(offset)
    var i = offset + stride
    var len = 1
    while (i < data.length && o.eqv(m, data(i))) {
      i += stride
      len += 1
    }
    len
  }

  final def partition[@spec A](data: Array[A], left: Int, right: Int, stride: Int)(m: A)(implicit o: Order[A]): Int = {
    var i = left  // Iterator.
    var j = left  // Pointer to first element > m.
    var k = left  // Pointer to end of equal elements.
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

trait MutatingMedianOf5 {
  final def mo5[@spec A](data: Array[A], offset: Int, stride: Int)(implicit o: Order[A]): Unit = {
    var i0 = offset
    var i1 = offset + 1 * stride
    var i2 = offset + 2 * stride
    var i3 = offset + 3 * stride
    var i4 = offset + 4 * stride
    var t = i0

    if (o.gt(data(i3), data(i4))) { t = i3; i3 = i4; i4 = t }
    if (o.gt(data(i1), data(i2))) { t = i1; i1 = i2; i2 = t }
    val i = if (o.lt(data(i4), data(i2))) {
      // Ignore 2. 3 < 4.
      if (o.lt(data(i1), data(i0))) { t = i0; i0 = i1; i1 = t }
      if (o.lt(data(i4), data(i1))) {
        // Ignore 1. 3 < 4
        if (o.lt(data(i4), data(i0))) i0 else i4
      } else {
        // Ignore 4. 0 < 1
        if (o.lt(data(i3), data(i1))) i1 else i3
      }
    } else {
      // Ignore 4. 1 < 2.
      if (o.lt(data(i3), data(i0))) { t = i0; i0 = i3; i3 = t }
      if (o.lt(data(i3), data(i2))) {
        // Ignore 2. 0 < 3
        if (o.lt(data(i3), data(i1))) i1 else i3
      } else {
        // Ignore 3. 1 < 2
        if (o.lt(data(i2), data(i0))) i0 else i2
      }
    }

    val m = data(i)
    data(i) = data(offset)
    data(offset) = m
  }
}

trait HighBranchingMedianOf5 {

  // Benchmarks show that this is slightly faster than the version above.

  // scalastyle:off method.length
  final def mo5[@spec A](data: Array[A], offset: Int, stride: Int)(implicit o: Order[A]): Unit = {
    val ai1 = data(offset)
    val ai2 = data(offset + stride)
    val ai3 = data(offset + 2 * stride)
    val ai4 = data(offset + 3 * stride)
    val ai5 = data(offset + 4 * stride)

    val i = if (o.lt(ai1, ai2)) {            // i1 < i2
      if (o.lt(ai3, ai4)) {          // i1 < i2, i3 < i4
        if (o.lt(ai2, ai4)) {        // Drop i4
          if (o.lt(ai3, ai5)) {      // i1 < i2, i3 < i5
            if (o.lt(ai2, ai5)) {    // Drop i5
              if (o.lt(ai2, ai3)) (offset + 2 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai1, ai5)) (offset + 4 * stride) else (offset + 0 * stride)
            }
          } else {                 // i1 < i2, i5 < i3
            if (o.lt(ai2, ai3)) {    // Drop i3
              if (o.lt(ai2, ai5)) (offset + 4 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai1, ai3)) (offset + 2 * stride) else (offset + 0 * stride)
            }
          }
        } else {                   // Drop i2
          if (o.lt(ai1, ai5)) {      // i1 < i5, i3 < i4
            if (o.lt(ai5, ai4)) {    // Drop i4
              if (o.lt(ai5, ai3)) (offset + 2 * stride) else (offset + 4 * stride)
            } else {                // Drop i5
              if (o.lt(ai1, ai4)) (offset + 3 * stride) else (offset + 0 * stride)
            }
          } else {                 // i5 < i1, i3 < i4
            if (o.lt(ai1, ai4)) {    // Drop i4
              if (o.lt(ai1, ai3)) (offset + 2 * stride) else (offset + 0 * stride)
            } else {                // Drop i1
              if (o.lt(ai5, ai4)) (offset + 3 * stride) else (offset + 4 * stride)
            }
          }
        }
      } else {                     // i1 < i2, i4 < i3
        if (o.lt(ai2, ai3)) {        // Drop i3
          if (o.lt(ai4, ai5)) {      // i1 < i2, i4 < i5
            if (o.lt(ai2, ai5)) {    // Drop i5
              if (o.lt(ai2, ai4)) (offset + 3 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai1, ai5)) (offset + 4 * stride) else (offset + 0 * stride)
            }
          } else {                 // i1 < i2, i5 < i4
            if (o.lt(ai2, ai4)) {    // Drop i4
              if (o.lt(ai2, ai5)) (offset + 4 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai1, ai4)) (offset + 3 * stride) else (offset + 0 * stride)
            }
          }
        } else {                   // Drop i2
          if (o.lt(ai1, ai5)) {      // i1 < i5, i4 < i3
            if (o.lt(ai5, ai3)) {    // Drop i3
              if (o.lt(ai5, ai4)) (offset + 3 * stride) else (offset + 4 * stride)
            } else {                // Drop i5
              if (o.lt(ai1, ai3)) (offset + 2 * stride) else (offset + 0 * stride)
            }
          } else {                 // i5 < i1, i4 < i3
            if (o.lt(ai1, ai3)) {    // Drop i3
              if (o.lt(ai1, ai4)) (offset + 3 * stride) else (offset + 0 * stride)
            } else {               // Drop i1
              if (o.lt(ai5, ai3)) (offset + 2 * stride) else (offset + 4 * stride)
            }
          }
        }
      }
    } else {                       // i2 < i1
      if (o.lt(ai3, ai4)) {          // i2 < i1, i3 < i4
        if (o.lt(ai1, ai4)) {        // Drop i4
          if (o.lt(ai3, ai5)) {      // i2 < i1, i3 < i5
            if (o.lt(ai1, ai5)) {    // Drop i5
              if (o.lt(ai1, ai3)) (offset + 2 * stride) else (offset + 0 * stride)
            } else {               // Drop i1
              if (o.lt(ai2, ai5)) (offset + 4 * stride) else (offset + 1 * stride)
            }
          } else {                 // i2 < i1, i5 < i3
            if (o.lt(ai1, ai3)) {    // Drop i3
              if (o.lt(ai1, ai5)) (offset + 4 * stride) else (offset + 0 * stride)
            } else {               // Drop i1
              if (o.lt(ai2, ai3)) (offset + 2 * stride) else (offset + 1 * stride)
            }
          }
        } else {                   // Drop i1
          if (o.lt(ai2, ai5)) {      // i2 < i5, i3 < i4
            if (o.lt(ai5, ai4)) {    // Drop i4
              if (o.lt(ai5, ai3)) (offset + 2 * stride) else (offset + 4 * stride)
            } else {                // Drop i5
              if (o.lt(ai2, ai4)) (offset + 3 * stride) else (offset + 1 * stride)
            }
          } else {                 // i5 < i2, i3 < i4
            if (o.lt(ai2, ai4)) {    // Drop i4
              if (o.lt(ai2, ai3)) (offset + 2 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai5, ai4)) (offset + 3 * stride) else (offset + 4 * stride)
            }
          }
        }
      } else {                     // i2 < i1, i4 < i3
        if (o.lt(ai1, ai3)) {        // Drop i3
          if (o.lt(ai4, ai5)) {      // i2 < i1, i4 < i5
            if (o.lt(ai1, ai5)) {    // Drop i5
              if (o.lt(ai1, ai4)) (offset + 3 * stride) else (offset + 0 * stride)
            } else {               // Drop i1
              if (o.lt(ai2, ai5)) (offset + 4 * stride) else (offset + 1 * stride)
            }
          } else {                 // i2 < i1, i5 < i4
            if (o.lt(ai1, ai4)) {    // Drop i4
              if (o.lt(ai1, ai5)) (offset + 4 * stride) else (offset + 0 * stride)
            } else {                // Drop i1
              if (o.lt(ai2, ai4)) (offset + 3 * stride) else (offset + 1 * stride)
            }
          }
        } else {                   // Drop i1
          if (o.lt(ai2, ai5)) {      // i2 < i5, i4 < i3
            if (o.lt(ai5, ai3)) {    // Drop i3
              if (o.lt(ai5, ai4)) (offset + 3 * stride) else (offset + 4 * stride)
            } else {                // Drop i5
              if (o.lt(ai2, ai3)) (offset + 2 * stride) else (offset + 1 * stride)
            }
          } else {                 // i5 < i2, i4 < i3
            if (o.lt(ai2, ai3)) {    // Drop i3
              if (o.lt(ai2, ai4)) (offset + 3 * stride) else (offset + 1 * stride)
            } else {               // Drop i2
              if (o.lt(ai5, ai3)) (offset + 2 * stride) else (offset + 4 * stride)
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

  final def approxMedian[@spec A: Order](data: Array[A], left: Int, right: Int, stride: Int): A = {
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

  final def approxMedian[@spec A: Order](data: Array[A], left: Int, right: Int, stride: Int): A = {
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
  final def select[@spec A: Order: ClassTag](data: Array[A], k: Int): Unit =
    quickSelect(data, k)

  final def linearSelect[@spec A: Order: ClassTag](data: Array[A], k: Int): Unit =
    LinearSelect.select(data, k)

  final def quickSelect[@spec A: Order: ClassTag](data: Array[A], k: Int): Unit =
    QuickSelect.select(data, k)
}
