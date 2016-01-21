package spire.macros

import org.scalatest.Matchers

import org.scalatest._

/**
 * Verifies that this macro will only compile successfully for `Constant` AST
 * values that are > 0.
 */
class PositiveIntTest extends FunSuite with Matchers {
  test("PositiveInt(n) does not compile for literal n <= 0") {
    "PositiveInt(0)" shouldNot compile
    "PositiveInt(-1)" shouldNot compile
    "PositiveInt(-123421)" shouldNot compile
    "PositiveInt(Int.MinValue)" shouldNot compile
  }

  test("PositiveInt(n) works for n > 0") {
    PositiveInt(1).value shouldBe 1
    PositiveInt(1234321).value shouldBe 1234321
    PositiveInt(Int.MaxValue).value shouldBe Int.MaxValue
  }

  test("PositiveInt.check(n) throws IllegalArgumentException for n <= 0") {
    an [IllegalArgumentException] should be thrownBy { PositiveInt.check(0) }
    an [IllegalArgumentException] should be thrownBy { PositiveInt.check(-1) }
    an [IllegalArgumentException] should be thrownBy { PositiveInt.check(-1234321) }
    an [IllegalArgumentException] should be thrownBy { PositiveInt.check(Int.MinValue) }
  }

  test("PositiveInt.check(n) works for n > 0") {
    PositiveInt.check(1).value shouldBe 1
    PositiveInt.check(1234321).value shouldBe 1234321
    PositiveInt.check(Int.MaxValue).value shouldBe Int.MaxValue
  }
}
