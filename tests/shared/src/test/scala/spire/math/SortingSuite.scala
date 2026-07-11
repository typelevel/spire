/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.algebra.Order
import spire.math.Sorting.{insertionSort, mergeSort, quickSort}
import spire.std.int._

class SortingSuite extends munit.FunSuite {

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
    matchAgainstExpectedForEachSortMethod(Array.fill(20) { 7 }, Array.fill(20) { 7 })
  }

  test("Sort a list of strings") {
    matchAgainstExpectedForEachSortMethod(Array("There", "is", "a", "light", "that", "never", "goes", "out"),
                                          Array("There", "a", "goes", "is", "light", "never", "out", "that")
    )
  }

  test("Merge and insertion sorts are stable") {
    // This will fail for quickSort
    checkStability(mergeSort)
    checkStability(insertionSort)
  }

  test("Use InsertionSort to sort a specific range") {
    val rangeToSort = Array(7, 6, 4)
    val expectedAfter = Array(4, 6, 7)

    matchAgainstExpected[Int](InsertionSort.sort(_, 2, 5),
                              Array(5, 8) ++ rangeToSort ++ Array(1, -1, 3),
                              Array(5, 8) ++ expectedAfter ++ Array(1, -1, 3)
    )
  }

  test("Use InsertionSort to sort an empty range") {
    matchAgainstExpected[Int](InsertionSort.sort(_, 2, 2),
                              Array(5, 8, 7, 6, 4, 1, -1, 3),
                              Array(5, 8, 7, 6, 4, 1, -1, 3)
    )
  }

  test("InsertionSort: start must be less than or equal to end") {
    intercept[IllegalArgumentException] { InsertionSort.sort(Array(7, 6, 4, 2, 1), 4, 3) }
  }

  test("InsertionSort: start must be nonnegative") {
    intercept[IllegalArgumentException] { InsertionSort.sort(Array(7, 6, 4, 2, 1), -1, 3) }
  }

  test("InsertionSort: end cannot exceed the length of the input array") {
    intercept[IllegalArgumentException] { InsertionSort.sort(Array(7, 6, 4, 2, 1), 2, 6) }
  }

  test("MergeSort.merge merges segments of the input and writes to the output") {
    val segment1 = Array(2, 5, 13)
    val segment2 = Array(4, 3, 19)
    val expectedAfterMerging = Array(2, 4, 3, 5, 13, 19)

    matchAgainstExpected[Int](MergeSort.merge(Array(1) ++ segment1 ++ segment2 ++ Array(-2, 9), _, 1, 4, 7),
                              new Array[Int](9),
                              Array(0) ++ expectedAfterMerging ++ Array(0, 0)
    )
  }

  test("MergeSort.merge succeeds when start and end are the extreme allowed values") {
    matchAgainstExpected[Int](MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), _, 0, 4, 9),
                              new Array[Int](9),
                              Array(1, 2, 4, 3, 5, 13, 19, -2, 9)
    )
  }

  test("MergeSort.merge does nothing when start = mid = end") {
    matchAgainstExpected[Int](MergeSort.merge(Array(1, 2, 5, 13, 4, 3, 19, -2, 9), _, 4, 4, 4),
                              new Array[Int](9),
                              Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
  }

  test("MergeSort.merge: end cannot exceed the size of the output array") {
    intercept[IllegalArgumentException] { MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](4), 3, 7, 8) }
  }

  test("MergeSort.merge: end cannot exceed the size of the input array") {
    intercept[IllegalArgumentException] { MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 3, 7, 12) }
  }

  test("MergeSort.merge: start must be nonnegative") {
    intercept[IllegalArgumentException] { MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), -1, 2, 4) }
  }

  test("MergeSort.merge: start <= mid <= end") {
    intercept[IllegalArgumentException] { MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 3, 1, 5) }
    intercept[IllegalArgumentException] { MergeSort.merge(Array(5, 3, 4, 7, 5, 8, 9), new Array[Int](20), 1, 5, 3) }

  }

  test("Use QuickSort to sort a specific range: start is inclusive, end is exclusive") {
    val rangeToSort = Array(8, 7, 6, 4, 1, -1)
    val expectedAfter = Array(-1, 1, 4, 6, 7, 8)

    matchAgainstExpected[Int](QuickSort.qsort(_, 1, 7),
                              Array(5) ++ rangeToSort ++ Array(3) ++ ENOUGH_ZEROS,
                              Array(5) ++ expectedAfter ++ Array(3) ++ ENOUGH_ZEROS
    )
  }

  test("Use QuickSort to sort an empty range") {
    matchAgainstExpected[Int](QuickSort.qsort(_, 2, 2),
                              Array(5, 8, 7, 6, 4, 1, -1, 3) ++ ENOUGH_ZEROS,
                              Array(5, 8, 7, 6, 4, 1, -1, 3) ++ ENOUGH_ZEROS
    )
  }

  test("QuickSort: start must be less than or equal to end") {
    intercept[IllegalArgumentException] { QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ ENOUGH_ZEROS, 4, 3) }
  }

  test("QuickSort: start must be nonnegative") {
    intercept[IllegalArgumentException] { QuickSort.qsort(Array(7, 6, 4, 2, 1) ++ ENOUGH_ZEROS, -1, 16) }
  }

  test("QuickSort: end cannot exceed the length of the input array") {
    val input = Array(7, 6, 4, 2, 1) ++ ENOUGH_ZEROS
    intercept[IllegalArgumentException] { QuickSort.qsort(input, 2, input.length + 1) }
  }

  test("QuickSort: Partitioning an array") {
    val leftSegment = Array(5, 11, 2)
    val rightSegment = Array(8, 1, 9)
    val pivotValue = 7
    val expectedAfterPartition = Array(5, 2, 1, 7, 8, 11, 9)

    matchAgainstExpected[Int](
      QuickSort.partition(_, 2, 9, 5),
      Array(6, -1) ++ leftSegment ++ Array(pivotValue) ++ rightSegment ++ Array(2, 10),
      Array(6, -1) ++ expectedAfterPartition ++ Array(2, 10)
    )
  }

  test("QuickSort.partition: 0 <= start <= pivot < end <= length of input") {
    val input = Array(2, 3, 4, 6)
    Array((-1, 2, 1), (0, 5, 2), (0, 4, 4), (2, 4, 1), (1, 3, 4)).foreach { case (start, end, pivotIndex) =>
      intercept[IllegalArgumentException] { QuickSort.partition(input, start, end, pivotIndex) }
    }
  }

  private val BIG = max(QuickSort.limit, MergeSort.startStep) * 2

  private val ENOUGH_ZEROS = new Array[Int](BIG) // For forcing the quick sort and merge sort implementations to kick in

  private def isSorted(input: Array[Int]): Unit = {
    assert(input.zip(input.tail).forall { case (p, q) => p <= q })
  }

  private case class Point(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
  }

  implicit private object PointOrder extends Order[Point] {
    override def compare(a: Point, b: Point): Int = a.x.compareTo(b.x)
  }

  private def arrayOfPoints(coordinates: (Int, Int)*) = coordinates.map { case (x, y) => Point(x, y) }.toArray

  implicit private object StringOrder extends Order[String] {
    override def compare(x: String, y: String): Int = x.compareTo(y)
  }

  private def checkStability(sortMethod: Array[Point] => Unit) = {
    val toSort = arrayOfPoints((1, 2),
                               (-1, 4),
                               (1, -20),
                               (3, 5),
                               (1, -10),
                               (4, 1),
                               (4, 3),
                               (5, 2),
                               (-6, 3),
                               (10, 10),
                               (4, -5),
                               (23, -23),
                               (0, 0),
                               (6, 7),
                               (7, 6),
                               (1, -1),
                               (-1, 8),
                               (4, 4),
                               (3, 3),
                               (2, 2)
    )

    val expectedAfter = arrayOfPoints((-6, 3),
                                      (-1, 4),
                                      (-1, 8),
                                      (0, 0),
                                      (1, 2),
                                      (1, -20),
                                      (1, -10),
                                      (1, -1),
                                      (2, 2),
                                      (3, 5),
                                      (3, 3),
                                      (4, 1),
                                      (4, 3),
                                      (4, -5),
                                      (4, 4),
                                      (5, 2),
                                      (6, 7),
                                      (7, 6),
                                      (10, 10),
                                      (23, -23)
    )

    matchAgainstExpected(sortMethod, toSort, expectedAfter)
  }

  private def matchAgainstExpected[A: Order: ClassTag](mutation: Array[A] => Unit,
                                                       toMutate: Array[A],
                                                       expectedAfter: Array[A]
  ) = {
    checkMutation(mutation, toMutate) { a =>
      assertEquals(a.toList, expectedAfter.toList)
    }
  }

  private def matchAgainstExpectedForEachSortMethod[A: Order: ClassTag](toMutate: Array[A], expected: Array[A]) = {
    checkAllSortMethods(toMutate) { (a: Array[A]) =>
      assertEquals(a.toList, expected.toList)
    }
  }

  private def checkAllSortMethods[A: Order: ClassTag](toSort: => Array[A])(check: Array[A] => Unit) = {
    checkMutation(mergeSort[A], toSort)(check)
    checkMutation(quickSort[A], toSort)(check)
    checkMutation(insertionSort[A], toSort)(check)
  }

  private def checkMutation[A: Order: ClassTag](mutation: Array[A] => Unit, toMutate: Array[A])(
    check: Array[A] => Unit
  ) = {
    val copyForMutation =
      toMutate.clone() // To avoid interdependence between tests, it's important that we clone the input array.

    mutation(copyForMutation)

    check(copyForMutation)
  }
}
