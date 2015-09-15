package spire.macros

import org.scalatest.Matchers

import org.scalatest._


/**
 * Verifies that this macro will only compile successfully for `Constant` AST
 * values that are > 0.
 */
class PositiveIntTest extends FunSuite with Matchers {

  test("Constructing a new instance with a negative value using the case class directly") {
    intercept[IllegalArgumentException] {
      PositiveInt(-10)
    }
  }

  test("Constructing a new instance with a negative value using the companion object's method ") {
      """ import spire.macros.PositiveInt; PositiveInt.build(-555) """.shouldNot(compile)
  }

  test("Constructing a new instance with a positive value using the companion object's method ") {
    """ import spire.macros.PositiveInt; PositiveInt.build(42) """.should(compile)
  }

}
