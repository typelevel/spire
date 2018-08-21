package spire
package math

import org.scalatest._
import spire.algebra.Order
import spire.math.Sorting.{insertionSort, mergeSort, quickSort}
import spire.std.int._

class SortingTest extends FunSuite with Matchers {

  test("The sort methods can handle empty arrays") {
    checkAllSortMethods(Array[Int]()){sorted => sorted should contain theSameElementsInOrderAs Array[Int]()}
  }

  test("The sort methods can handle singleton arrays") {
    checkAllSortMethods(Array("lonely")){sorted => sorted should contain theSameElementsInOrderAs Array("lonely")}
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
    checkAllSortMethods(Array(5, 4, 3, 2, 1)){sorted => sorted should contain theSameElementsInOrderAs Array(1, 2, 3, 4, 5)}
  }

  test("Sort a constant sequence") {
    checkAllSortMethods(Array.fill(20){7}) {sorted => sorted should contain theSameElementsInOrderAs Array.fill(20){7}}
  }

  test("Sort a list of strings") {
    checkAllSortMethods(Array("There", "is", "a", "light", "that", "never", "goes", "out")){
      sorted => sorted should contain theSameElementsInOrderAs Array("There", "a", "goes", "is", "light", "never", "out", "that")
    }
  }

  test("Merge and insertion sorts are stable") {
    // This will fail for quickSort
    checkStability(mergeSort)
    checkStability(insertionSort)
  }

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
