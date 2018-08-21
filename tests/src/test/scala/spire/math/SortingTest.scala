package spire
package math

import org.scalatest._
import spire.algebra.Order
import spire.math.Sorting.{insertionSort, mergeSort, quickSort}
import spire.std.int._

class SortingTest extends FunSuite with Matchers {

  test("The sort methods can handle empty arrays") {
    matchAgainstExpectedForEachSortMethod(Array[Int](), Array[Int]())
  }

  test("The sort methods can handle singleton arrays") {
    matchAgainstExpectedForEachSortMethod(Array("lonely"), Array("lonely"))
  }

  test("Sort randomly generated arrays of various sizes") {
    val smallSizes = Seq.range(1, 65)
    val randomGen = new scala.util.Random()

    smallSizes.foreach { size =>
      val input: Array[Int] = Array.tabulate(size) { _ => randomGen.nextInt(10000) }
      checkAllSortMethods(input)(isSorted)
    }

    val largeSizes = Seq(256, 1024, 9121)
    largeSizes.foreach { size =>
      val input: Array[Int] = Array.tabulate(size) { _ => randomGen.nextInt(10000) }
      checkMutation(mergeSort[Int], input)(isSorted)
      checkMutation(quickSort[Int], input)(isSorted)
    }
  }

  test("Sort a decreasing sequence") {
    matchAgainstExpectedForEachSortMethod(Array.range(0, BIG).reverse, Array.range(0, BIG))
  }

  test("Sort a constant sequence") {
    matchAgainstExpectedForEachSortMethod(Array.fill(20) {
      7
    }, Array.fill(20) {
      7
    })
  }

  test("Sort a list of strings") {
    matchAgainstExpectedForEachSortMethod(Array("There", "is", "a", "light", "that", "never", "goes", "out"),
      Array("There", "a", "goes", "is", "light", "never", "out", "that"))
  }

  test("Merge and insertion sorts are stable") {
    // This will fail for quickSort
    checkStability(mergeSort)
    checkStability(insertionSort)
  }

  test("Use InsertionSort to sort a specific range") {
    matchAgainstExpected[Int](InsertionSort.sort(_, 2, 5), Array(5, 8, 7, 6, 4, 1, -1, 3),
      Array(5, 8, 4, 6, 7, 1, -1, 3))
  }

  test("Use InsertionSort to sort an empty range") {
    matchAgainstExpected[Int](InsertionSort.sort(_, 2, 2), Array(5, 8, 7, 6, 4, 1, -1, 3),
      Array(5, 8, 7, 6, 4, 1, -1, 3))
  }

  test("InsertionSort: start must be less than or equal to end") {
    an[IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), 4, 3)
  }

  test("InsertionSort: start must be nonnegative") {
    an[IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), -1, 3)
  }

  test("InsertionSort: end cannot exceed the length of the input array") {
    an[IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), 2, 6)
  }

  test("MergeSort.merge merges segments of the input and writes to the output") {
    matchAgainstExpected[Int](MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), _, 1, 4, 7),
      new Array[Int](9), Array(0, 2, 4, 3, 5, 13, 19, 0, 0))
  }

  test("MergeSort.merge succeeds when start and end are the extreme allowed values") {
    matchAgainstExpected[Int](MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), _, 0, 4, 9),
      new Array[Int](9), Array(1, 2, 4, 3, 5, 13, 19, -2, 9))
  }

  test("MergeSort.merge does nothing when start = mid = end") {
    matchAgainstExpected[Int](MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), _, 4, 4, 4),
      new Array[Int](9), Array(0, 0, 0, 0, 0, 0, 0, 0, 0))
  }


  test("MergeSort.merge: end cannot exceed the size of the output array") {
    an[IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9),
      new Array[Int](4), 3, 7, 8)
  }

  test("MergeSort.merge: end cannot exceed the size of the input array") {
    an[IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9),
      new Array[Int](20), 3, 7, 12)
  }

  test("MergeSort.merge: start must be nonnegative") {
    an[IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9),
      new Array[Int](20), -1, 2, 4)
  }

  test("MergeSort.merge: start <= mid <= end") {
    an[IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9),
      new Array[Int](20), 3, 1, 5)
    an[IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9),
      new Array[Int](20), 1, 5, 3)

  }

  test("Use QuickSort to sort a specific range: start is inclusive, end is exclusive") {
    matchAgainstExpected[Int](QuickSort.qsort(_, 1, 7), Array(5, 8, 7, 6, 4, 1, -1, 3) ++ BIG_PADDING,
      Array(5, -1, 1, 4, 6, 7, 8, 3) ++ BIG_PADDING)
  }

  test("Use QuickSort to sort an empty range") {
    matchAgainstExpected[Int](QuickSort.qsort(_, 2, 2), Array(5, 8, 7, 6, 4, 1, -1, 3) ++ BIG_PADDING,
      Array(5, 8, 7, 6, 4, 1, -1, 3) ++ BIG_PADDING)
  }

  test("QuickSort: start must be less than or equal to end") {
    an[IllegalArgumentException] should be thrownBy QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ BIG_PADDING, 4, 3)
  }

  test("QuickSort: start must be nonnegative") {
    an[IllegalArgumentException] should be thrownBy QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ BIG_PADDING, -1, 16)
  }

  test("QuickSort: end cannot exceed the length of the input array") {
    val input = Array(7, 6, 4, 2, 1) ++ BIG_PADDING
    an[IllegalArgumentException] should be thrownBy QuickSort.qsort(input, 2, input.length + 1)
  }

  test("QuickSort: Partitioning an array") {
    matchAgainstExpected[Int](QuickSort.partition(_, 2, 9, 5), Array(6, -1, 5, 11, 2, 7, 8, 1, 9, 2, 10),
      Array(6, -1, 5, 2, 1, 7, 8, 11, 9, 2, 10))
  }

  test("QuickSort.partition: 0 <= start <= pivot < end <= length of input") {
    val input = Array(2, 3, 4, 6)
    Array((-1, 2, 1), (0, 5, 2), (0, 4, 4), (2, 4, 1), (1, 3, 4)).foreach {
      case (start, end, pivotIndex) => an [IllegalArgumentException] should be thrownBy QuickSort.partition(input, start, end, pivotIndex)
    }
  }

  private val BIG = max(QuickSort.limit, MergeSort.startStep) * 2

  private val BIG_PADDING = new Array[Int](BIG)

  private def isSorted(input: Array[Int]): Assertion = {
    assert(input.zip(input.tail).forall({ case (p, q) => p <= q }))
  }

  private case class Point(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
  }

  private implicit object PointOrder extends Order[Point] {
    override def compare(a: Point, b: Point): Int = a.x.compareTo(b.x)
  }

  private def arrayOfPoints(coordinates: (Int, Int)*) = (coordinates map { case (x, y) => Point(x, y) }).toArray


  private implicit object StringOrder extends Order[String] {
    override def compare(x: String, y: String): Int = x.compareTo(y)
  }

  private def checkStability(sortMethod: Array[Point] => Unit) = {
    val toSort = arrayOfPoints((1, 2), (-1, 4), (1, -20), (3, 5), (1, -10), (4, 1),
      (4, 3), (5, 2), (-6, 3), (10, 10), (4, -5), (23, -23), (0, 0),
      (6, 7), (7, 6), (1, -1), (-1, 8), (4, 4), (3, 3), (2, 2))

    val expectedAfter = arrayOfPoints((-6, 3), (-1, 4), (-1, 8), (0, 0), (1, 2), (1, -20),
      (1, -10), (1, -1), (2, 2), (3, 5), (3, 3), (4, 1), (4, 3), (4, -5),
      (4, 4), (5, 2), (6, 7), (7, 6), (10, 10), (23, -23))

    matchAgainstExpected(sortMethod, toSort, expectedAfter)
  }

  private def matchAgainstExpected[A: Order : ClassTag](mutation: Array[A] => Unit, toMutate: Array[A], expectedAfter: Array[A]) = {
    checkMutation(mutation, toMutate) {
      _ should contain theSameElementsInOrderAs expectedAfter
    }
  }

  private def matchAgainstExpectedForEachSortMethod[A: Order : ClassTag](toMutate: Array[A], expected: Array[A]) = {
    checkAllSortMethods(toMutate) {
      _ should contain theSameElementsInOrderAs expected
    }
  }

  private def checkAllSortMethods[A: Order : ClassTag](toSort: => Array[A])(check: Array[A] => Assertion) = {
    checkMutation(mergeSort[A], toSort)(check)
    checkMutation(quickSort[A], toSort)(check)
    checkMutation(insertionSort[A], toSort)(check)
  }

  private def checkMutation[A: Order : ClassTag](mutation: Array[A] => Unit, toMutate: Array[A])(check: Array[A] => Assertion) = {
    val copyForMutation = toMutate.clone() // To avoid interdependence between tests, it's important that we clone the input array.

    mutation(copyForMutation)

    check(copyForMutation)
  }
}
