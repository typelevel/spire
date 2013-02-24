package spire.math

import scala.{specialized => spec}
import scala.annotation.tailrec

object Searching {
  final def search[@spec A: Order](as: Array[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[@spec A](as: Array[A], item: A, lower: Int, upper: Int)(implicit ev: Order[A]): Int = {
    var first = lower
    var last = upper
    while (first <= last) {
      val middle = (first + last) >>> 1
      val m = as(middle)
      if (ev.lt(m, item)) {
        first = middle + 1
      } else if (ev.gt(m, item)) {
        last = middle - 1
      } else {
        return middle
      }
    }
    -first - 1
  }

  final def search[A: Order](as: IndexedSeq[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[A](as: IndexedSeq[A], item: A, lower: Int, upper: Int)(implicit ev: Order[A]): Int = {
    var first = lower
    var last = upper
    while (first <= last) {
      val middle = (first + last) >>> 1
      val m = as(middle)
      if (ev.lt(m, item)) {
        first = middle + 1
      } else if (ev.gt(m, item)) {
        last = middle - 1
      } else {
        return middle
      }
    }
    -first - 1
  }
}
