package numerics.math

import numerics.math.fun._

import org.scalatest.FunSuite

class SortingTest extends FunSuite {
  test("sort()") {
    val before = Array(23, 1, 52, 64, 234, 623, 124, 421, 421)

    val goal = before.clone()
    scala.util.Sorting.quickSort(goal)

    val merged = before.clone()
    Sorting.mergeSort(merged)

    val quicked = before.clone()
    Sorting.mergeSort(quicked)

    // make sure our result is ok
    for (i <- 0 until goal.length) assert(merged(i) === goal(i))
    for (i <- 0 until goal.length) assert(quicked(i) === goal(i))
  }
}
