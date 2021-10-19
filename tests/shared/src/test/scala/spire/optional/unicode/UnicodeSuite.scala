package spire.optional.unicode

import spire.implicits._

class UnicodeSuite extends munit.FunSuite {
  test("Sanity test") {
    // Basically we want to ensure the module is present in scala-2 and scala-3
    assertEquals(√(4.0), 2.0)
    assert(2.00 ≡ 2.00)
  }
}
