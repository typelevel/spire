package spire.math

import spire.algebra.Order
import spire.syntax.order._

import scala.{specialized => spec}
import scala.annotation.tailrec

object Searching {
  final def search[@spec A: Order](as: Array[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[@spec A: Order](as: Array[A], item: A, lower: Int, upper: Int): Int = {
    var first = lower
    var last = upper
    while (first <= last) {
      val middle = (first + last) >>> 1

      val compare = as(middle).compare(item)
      if (compare < 0) first = middle + 1
      else if (compare > 0) last = middle - 1
      else return middle
    }
    -first - 1
  }

  final def search[@spec A: Order](as: IndexedSeq[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[@spec A: Order](as: IndexedSeq[A], item: A, lower: Int, upper: Int): Int = {
    var first = lower
    var last = upper
    while (first <= last) {
      val middle = (first + last) >>> 1

      val compare = as(middle).compare(item)
      if (compare < 0) first = middle + 1
      else if (compare > 0) last = middle - 1
      else return middle
    }
    -first - 1
  }
}
