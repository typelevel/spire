package spire
package math

import spire.implicits._

class SearchSuite extends munit.FunSuite {
  test("search when type provides a traditional comparison method") {
    assertEquals(Searching.search(Array(1, 2, 3), 0), -1)
    assertEquals(Searching.search(Array(1, 2, 3), 2), 1)
  }

  test("search when comparison can be a value not in {-1, 0, 1}") {
    assert(Searching.search(Array("a", "b", "c"), "aaa") < 0)
    assertEquals(Searching.search(Array("a", "b", "c"), "b"), 1)
  }

  test("search indexedSeq when type provides a traditional comparison method") {
    assertEquals(Searching.search(IndexedSeq(1, 2, 3), 0), -1)
    assertEquals(Searching.search(IndexedSeq(1, 2, 3), 2), 1)
  }

  test("search indexedSeq when comparison can be a value not in {-1, 0, 1}") {
    assert(Searching.search(IndexedSeq("a", "b", "c"), "aaa") < 0)
    assertEquals(Searching.search(IndexedSeq("a", "b", "c"), "b"), 1)
  }

}
