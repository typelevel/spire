package spire
package math

import spire.algebra.{Order, PartialOrder}
import spire.syntax.order._


object Searching {
  final def search[@sp A: Order](as: Array[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[@sp A: Order](as: Array[A], item: A, lower: Int, upper: Int): Int = {
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

  final def search[@sp A: Order](as: IndexedSeq[A], item: A): Int =
    search(as, item, 0, as.length - 1)

  final def search[@sp A: Order](as: IndexedSeq[A], item: A, lower: Int, upper: Int): Int = {
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

  /** Computes the minimal elements of a partially ordered set.
    * If the poset contains multiple copies of a minimal element, the function
    * will only return a single copy of it.
    *
    * Works by constructing the set of minimal elements for the first k elements
    * of the poset, for k = 1 .. length.
    *
    * With n is the size of the poset and w <= n its width, the algorithm requires
    * O(w) space and O(w*n) time.
    */
  final def minimalElements[A](as: Iterable[A])(implicit ev: PartialOrder[A]): Seq[A] = {
    import scala.collection.mutable.ArrayBuffer
    // the minimal elements for the first elements of as
    var candidates = ArrayBuffer.empty[A]
    // removes the j-th element of candidates by swapping the last
    // element with it
    def fastRemove(j: Int): Unit = {
      if (j < candidates.length - 1)
        candidates(j) = candidates(candidates.length - 1)
      candidates.remove(candidates.length - 1)
    }
    as.foreach { a =>
      // if we prove that a is not minimal, it should not be added to candidates
      var aIsNotMinimal = false
      // compare a against each candidate, starting from the last
      @tailrec def inspect(i: Int): Unit = {
        if (i >= 0) {
          val c = a.partialCompare(candidates(i))
          // if a <= candidates(i), this candidate can be removed
          if (c <= 0.0) {
            fastRemove(i)
            // inspect the next candidate, taking care of the state of candidates
            // after the removal
            if (i < candidates.length)
              inspect(i)
            else
              inspect(i - 1)
          } else if (c > 0.0) {
            // if a > candidates(i), then a is not minimal
            aIsNotMinimal = true
            inspect(i - 1)
          } else // if a cannot be compare to candidates(i), continue inspection
            inspect(i - 1)
        }
      }
      inspect(candidates.length - 1)
      // if a is minimal, then add it to the candidates pool
      if (!aIsNotMinimal)
        candidates += a
    }
    Seq(candidates: _*)
  }
}
