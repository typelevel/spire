package spire.math

import org.scalatest.FunSuite

//import spire.algebra._
import spire.std.int._

class SortingTest extends FunSuite {
  def testSort(before: Array[Int]) = {

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
}
