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
      as(middle).compare(item) match {
        case -1 => first = middle + 1
        case 1 => last = middle - 1
        case 0 => return middle
      }
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
      as(middle).compare(item) match {
        case -1 => first = middle + 1
        case 1 => last = middle - 1
        case 0 => return middle
      }
    }
    -first - 1
  }
}
