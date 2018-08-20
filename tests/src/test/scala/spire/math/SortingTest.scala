package spire
package math

import org.scalatest._
import spire.algebra.Order
import spire.math.Sorting.{insertionSort, mergeSort, quickSort}
import spire.std.int._

class SortingTest extends FunSuite with Matchers {
  private def testSort(before: Array[Int]) = {

    val goal = before.clone()
    scala.util.Sorting.quickSort(goal)

    val merged = before.clone()
    mergeSort(merged)

    val quicked = before.clone()
    quickSort(quicked)

    // make sure our result is ok
    for (i <- 0 until before.length) assert(merged(i) === goal(i))
    for (i <- 0 until before.length) assert(quicked(i) === goal(i))
  }

  test("The sort methods can handle empty arrays") {
    checkAllSortMethods(Array[Int](), Array[Int]())
  }

  test("The sort methods can handle singleton arrays") {
    checkAllSortMethods(Array("lonely"), Array("lonely"))
  }

  test("trivial sort") {
    testSort(Array(2, 1))
  }

  test("sort 3 decreasing") {
    testSort(Array(3, 2, 1))
  }

  test("sort()") {
    testSort(Array(23, 1, 52, 64, 234, 623, 124, 421, 421))
  }

  test("sort 5 decreasing") {
    testSort(Array(5, 4, 3, 2, 1))
  }

  test("Sort a list of strings") {
    checkAllSortMethods(Array("There", "is", "a", "light", "that", "never", "goes", "out"),
      Array("There", "a", "goes", "is", "light", "never", "out", "that"))
  }

  test("Merge and insertion sorts are stable") {
    // This will fail for quickSort
    checkStability(mergeSort)
    checkStability(insertionSort)
  }

  private case class Point(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
  }

  private implicit object PointOrder extends Order[Point] {
    override def compare(a: Point, b: Point): Int = a.x.compareTo(b.x)
  }

  private def arrayOfPoints(coordinates: (Int, Int)*) = (coordinates map { case (x, y) => Point(x, y) }).toArray

  private def checkStability(sortMethod: Array[Point] => Unit) = {
    val toSort = arrayOfPoints((1, 2), (-1, 4), (1, -20), (3, 5), (1, -10), (4, 1),
      (4, 3), (5, 2), (-6, 3), (10, 10), (4, -5), (23, -23), (0, 0),
      (6, 7), (7, 6), (1, -1), (-1, 8), (4, 4), (3, 3), (2, 2))

    val expectedAfter = arrayOfPoints((-6, 3), (-1, 4), (-1, 8), (0, 0), (1, 2), (1, -20),
      (1, -10), (1, -1), (2, 2), (3, 5), (3, 3), (4, 1), (4, 3), (4, -5),
      (4, 4), (5, 2), (6, 7), (7, 6), (10, 10), (23, -23))

    sortAndCheck(sortMethod, toSort, expectedAfter)
  }

  private implicit object StringOrder extends Order[String] {
    override def compare(x: String, y: String): Int = x.compareTo(y)
  }

  private def checkAllSortMethods[A: Order : ClassTag](toSort: => Array[A], expectedAfter: => Array[A]) = {
    sortAndCheck(mergeSort[A], toSort, expectedAfter)
    sortAndCheck(quickSort[A], toSort, expectedAfter)
    sortAndCheck(insertionSort[A], toSort, expectedAfter)
  }

  private def sortAndCheck[A: Order : ClassTag](sortMethod: Array[A] => Unit, toSort: Array[A], expectedAfter: Array[A]) = {
    val copyForSorting = toSort.clone() // To avoid interdependence between tests, it's important that we clone the input array.

    sortMethod(copyForSorting)

    copyForSorting should contain theSameElementsInOrderAs expectedAfter
  }
}
