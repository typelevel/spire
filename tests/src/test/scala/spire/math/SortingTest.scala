package spire
package math

import org.scalatest._
import spire.algebra.Order
import spire.std.int._

class SortingTest extends FunSuite with Matchers {
  private def testSort(before: Array[Int]) = {

    val goal = before.clone()
    scala.util.Sorting.quickSort(goal)

    val merged = before.clone()
    Sorting.mergeSort(merged)

    val quicked = before.clone()
    Sorting.quickSort(quicked)

    // make sure our result is ok
    for (i <- 0 until before.length) assert(merged(i) === goal(i))
    for (i <- 0 until before.length) assert(quicked(i) === goal(i))
  }


  test("sort empty array") {
    testSort(Array[Int]())
  }

  test("sort singleton") {
    testSort(Array[Int](1))
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

  case class Point(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
  }

  implicit object PointOrder extends Order[Point] {
    override def compare(a: Point, b: Point): Int = a.x.compareTo(b.x)
  }

  private def arrayOfPoints(coordinates: (Int, Int)*) = (coordinates map {case (x, y) => Point(x, y)}).toArray

  test("Merge and insertion sorts are stable") {
    checkStability(Sorting.mergeSort[Point])
    checkStability(Sorting.insertionSort[Point])
  }


  private def checkStability(sortMethod: Array[Point] => Unit) = {
    val toSort = arrayOfPoints((1, 2), (-1, 4), (1, -20), (3, 5), (1, -10), (4, 1),
      (4, 3), (5, 2), (-6, 3), (10, 10), (4, -5), (23, -23), (0, 0),
      (6, 7), (7, 6), (1, -1), (-1, 8), (4, 4), (3, 3), (2, 2))

    val expectedAfter = arrayOfPoints((-6, 3), (-1, 4), (-1, 8), (0, 0), (1, 2), (1, -20),
      (1, -10), (1, -1), (2, 2), (3, 5), (3, 3), (4, 1), (4, 3), (4, -5),
      (4, 4), (5, 2), (6, 7), (7, 6), (10, 10), (23, -23))

    sortMethod(toSort)

    toSort should contain theSameElementsInOrderAs expectedAfter
  }
}
