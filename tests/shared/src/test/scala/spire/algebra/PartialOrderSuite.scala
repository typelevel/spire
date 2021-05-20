package spire
package algebra

class PartialOrderSuite extends munit.FunSuite {

  import spire.optional.powerSetPartialOrder._
  import spire.implicits._

  test("Minimal and maximal elements of {{1, 2, 3}, {3}, {2}, {1}} by power set partial order") {
    val sets = Seq(Set(1, 2, 3), Set(3), Set(2), Set(1), Set(1, 4))
    assertEquals(sets.pmin.toSet, Set(Set(1), Set(2), Set(3)))
    assertEquals(sets.pmax.toSet, Set(Set(1, 2, 3), Set(1, 4)))
  }
  test("Set(1, 2, 3) <= Set(1, 2, 3)") { assert(Set(1, 2, 3) <= Set(1, 2, 3)) }
  test("not Set(1, 2, 3) < Set(1, 2, 3)") { assert(!(Set(1, 2, 3) < Set(1, 2, 3))) }
  test("Set(1, 2, 3) > Set(1, 2)") { assert(Set(1, 2, 3) > Set(1, 2)) }
}
