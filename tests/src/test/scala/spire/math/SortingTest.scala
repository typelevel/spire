package spire
package math

import org.scalatest._
import spire.algebra.Order
import spire.math.Sorting.{insertionSort, mergeSort, quickSort}
import spire.std.int._

class SortingTest extends FunSuite with Matchers {

  test("The sort methods can handle empty arrays") {
    checkAllSortMethods(Array[Int]()){_ should contain theSameElementsInOrderAs Array[Int]()}
  }

  test("The sort methods can handle singleton arrays") {
    checkAllSortMethods(Array("lonely")){_ should contain theSameElementsInOrderAs Array("lonely")}
  }

  test("Sort randomly generated arrays of various sizes") {
    val sizes = Seq.range(1, 65) ++ Seq(256, 1024, 9121, 65539)
    val randomGen = new scala.util.Random()

    sizes.foreach { size =>
      val input: Array[Int] = Array.tabulate(size){_ => randomGen.nextInt(Int.MaxValue)}
      checkAllSortMethods(input)(isSorted)
    }
  }

  test("Sort a decreasing sequence") {
    checkAllSortMethods(Array.range(0, forceStrategySize).reverse){ _ should contain theSameElementsInOrderAs Array.range(0, forceStrategySize) }
  }

  test("Sort a constant sequence") {
    checkAllSortMethods(Array.fill(20){7}) { _ should contain theSameElementsInOrderAs Array.fill(20){7}}
  }

  test("Sort a list of strings") {
    checkAllSortMethods(Array("There", "is", "a", "light", "that", "never", "goes", "out")){
      _ should contain theSameElementsInOrderAs Array("There", "a", "goes", "is", "light", "never", "out", "that")
    }
  }

  test("Merge and insertion sorts are stable") {
    // This will fail for quickSort
    checkStability(mergeSort)
    checkStability(insertionSort)
  }

  test("Use InsertionSort to sort a specific range") {
    checkSortMethod[Int](InsertionSort.sort(_, 2, 5), Array(5, 8, 7, 6, 4, 1, -1, 3)) {
      _ should contain theSameElementsInOrderAs Array(5, 8, 4, 6, 7, 1, -1, 3)
    }
  }

  test("Use InsertionSort to sort an empty range") {
    checkSortMethod[Int](InsertionSort.sort(_, 2, 2), Array(5, 8, 7, 6, 4, 1, -1, 3)) {
      _ should contain theSameElementsInOrderAs Array(5, 8, 7, 6, 4, 1, -1, 3)
    }
  }

  test("InsertionSort: start must be less than or equal to end") {
    an [IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), 4, 3)
  }

  test("InsertionSort: start must be nonnegative") {
    an [IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), -1, 3)
  }

  test("InsertionSort: end cannot exceed the length of the input array") {
    an [IllegalArgumentException] should be thrownBy InsertionSort.sort(Array(7, 6, 4, 2, 1), 2, 6)
  }

  test("MergeSort.merge merges segments of the input and writes to the output") {
    val outputArray = new Array[Int](9)
    MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), outputArray, 1, 4, 7)
    outputArray should contain theSameElementsInOrderAs Array(0, 2, 4, 3, 5, 13, 19, 0, 0)
  }

  test("MergeSort.merge succeeds when start and end are the extreme allowed values") {
    val outputArray = new Array[Int](9)
    MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), outputArray, 0, 4, 9)
    outputArray should contain theSameElementsInOrderAs Array(1, 2, 4, 3, 5, 13, 19, -2, 9)
  }

  test("MergeSort.merge does nothing when start = mid = end") {
    val outputArray = new Array[Int](9)
    MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), outputArray, 4, 4, 4)
    outputArray should contain theSameElementsInOrderAs Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  }


  test("MergeSort.merge: end cannot exceed the size of the output array") {
    an [IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](4), 3, 7, 8)
  }

  test("MergeSort.merge: end cannot exceed the size of the input array") {
    an [IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 3, 7, 12)
  }

  test("MergeSort.merge: start must be nonnegative") {
    an [IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), -1, 2, 4)
  }

  test("MergeSort.merge: start <= mid <= end") {
    an [IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 3, 1, 5)
    an [IllegalArgumentException] should be thrownBy MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 1, 5, 3)

  }

  test("Use QuickSort to sort a specific range: start is inclusive, end is exclusive") {
    checkSortMethod[Int](QuickSort.qsort(_, 1, 7), Array(5, 8, 7, 6, 4, 1, -1, 3) ++ forceQuickSortPadding) {
      _ should contain theSameElementsInOrderAs Array(5, -1, 1, 4, 6, 7, 8, 3) ++ forceQuickSortPadding
    }
  }

  test("Use QuickSort to sort an empty range") {
    checkSortMethod[Int](QuickSort.qsort(_, 2, 2), Array(5, 8, 7, 6, 4, 1, -1, 3) ++ forceQuickSortPadding) {
      _ should contain theSameElementsInOrderAs Array(5, 8, 7, 6, 4, 1, -1, 3) ++ forceQuickSortPadding
    }
  }

  test("QuickSort: start must be less than or equal to end") {
    an [IllegalArgumentException] should be thrownBy QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ forceQuickSortPadding, 4, 3)
  }

  test("QuickSort: start must be nonnegative") {
    an [IllegalArgumentException] should be thrownBy QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ forceQuickSortPadding, -1, 16)
  }

  test("QuickSort: end cannot exceed the length of the input array") {
    val input = Array(7, 6, 4, 2, 1) ++ forceQuickSortPadding
    an [IllegalArgumentException] should be thrownBy QuickSort.qsort(input, 2, input.length + 1)
  }

  private val forceStrategySize = max(QuickSort.limit, MergeSort.startStep) * 2

  private val forceQuickSortPadding = new Array[Int](forceStrategySize)

  private def isSorted(input: Array[Int]): Assertion = {
    assert(input.zip(input.tail).forall({case (p, q) => p <= q}))
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

    checkSortMethod(sortMethod, toSort){_ should contain theSameElementsInOrderAs expectedAfter}
  }
  private def checkAllSortMethods[A: Order : ClassTag](toSort: => Array[A])(check: Array[A] => Assertion) = {
    checkSortMethod(mergeSort[A], toSort)(check)
    checkSortMethod(quickSort[A], toSort)(check)
    checkSortMethod(insertionSort[A], toSort)(check)
  }

  private def checkSortMethod[A: Order : ClassTag](sortMethod: Array[A] => Unit, toSort: Array[A])(check: Array[A] => Assertion) = {
    val copyForSorting = toSort.clone() // To avoid interdependence between tests, it's important that we clone the input array.

    sortMethod(copyForSorting)

    check(copyForSorting)
  }
}
