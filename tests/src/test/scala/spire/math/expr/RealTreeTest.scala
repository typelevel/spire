package spire.math
package expr

import org.scalatest.FunSuite
import org.scalatest.Matchers
import spire.implicits.{eqOps => _, _}
import java.math.MathContext

class RealTreeTest extends FunSuite with Matchers {
  test("parse sin(3.03) + 1/8 and verify value") {
    val v = RealTree.value(RealTree.parse("sin(3.03) + 1/8").get)
    v === sin(Real("3.03")) + 1/Real(8) shouldBe true
  }
  test("pretty print 3+(3*4) removes parentheses") {
    RealTree.print(RealTree.parse("3+(3*4)").get) shouldBe "3+3*4"
  }
  test("3+(3*4) simplifies to 15") {
    RealTree.print(RealTree.simplified(RealTree.parse("3+(3*4)").get)) shouldBe "15"
  }
  test("3+(3.*4) does not simplify") {
    RealTree.print(RealTree.simplified(RealTree.parse("3+(3.*4)").get)) shouldBe "3+3.*4"
  }
}
