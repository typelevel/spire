package spire.macros

import org.scalatest.Matchers

import org.scalatest._

/**
 * Verifies that this macro will only compile successfully for `Constant` AST
 * values that are > 0.
 */
class PositiveIntTest extends FunSuite with Matchers {

  test("Constructing with its companion method, 'build', should fail to compile with a negative numbers.") {
    "import spire.macros.PositiveInt; PositiveInt.build(-55)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-1)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-9999)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-1234)".shouldNot(compile)
  }

  test("Constructing a `PositiveInt` via its case class with a (0 :Int) should throw") {
    intercept[IllegalArgumentException] {
      PositiveInt(0)
    }
  }

}
