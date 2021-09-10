package spire
package algebra

class PartialOrderSuite extends munit.FunSuite {

  import spire.optional.powerSetPartialOrder._
  import spire.implicits._

  test("Minimal and maximal elements of {{1, 2, 3}, {3}, {2}, {1}} by power set partial order") {
    val sets = Seq(Set[Int](1, 2, 3), Set[Int](3), Set[Int](2), Set[Int](1), Set[Int](1, 4))
    assertEquals(seqOps(sets).pmin.toSet, Set(Set[Int](1), Set[Int](2), Set[Int](3)))
    assertEquals(seqOps(sets).pmax.toSet, Set(Set[Int](1, 2, 3), Set[Int](1, 4)))
  }
  test("Set(1, 2, 3) <= Set(1, 2, 3)") { assert(Set[Int](1, 2, 3) <= Set[Int](1, 2, 3)) }
  test("not Set(1, 2, 3) < Set(1, 2, 3)") { assert(!(Set[Int](1, 2, 3) < Set[Int](1, 2, 3))) }
  test("Set(1, 2, 3) > Set(1, 2)") { assert(Set[Int](1, 2, 3) > Set[Int](1, 2)) }
}
