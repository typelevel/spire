package spire.math

import org.scalatest.funsuite.AnyFunSuite
import spire.implicits.{ BigDecimalAlgebra, nrootOps }

class BigDecimalTest extends AnyFunSuite {
  test("sqrt") {
    // this sqrt used to infinite loop
    assert(BigDecimal("4.000000000000000000000000000000003").sqrt === BigDecimal("2.000000000000000000000000000000001"))
  }
}
