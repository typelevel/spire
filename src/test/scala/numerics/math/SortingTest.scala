package numerics.math

import numerics.math.fun._

import org.scalatest.FunSuite

class SortingTest extends FunSuite {
  test("sort()") {
    val data = Array(23, 1, 52, 64, 234, 623, 124, 421, 421)

    val goal = data.clone()
    scala.util.Sorting.quickSort(goal)

    val before = data.clone()
    val after = Sorting.sort(before)

    // make sure we didn't mutate things
    for (i <- 0 until data.length) assert(before(i) === data(i))

    // make sure our result is ok
    for (i <- 0 until data.length) assert(after(i) === goal(i))
  }
}
