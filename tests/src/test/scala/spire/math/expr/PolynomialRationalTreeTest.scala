package spire.math
package expr

import org.scalatest.FunSuite
import org.scalatest.Matchers
import spire.implicits.{eqOps => _, _}
import java.math.MathContext

class PolynomialRationalTreeTest extends FunSuite with Matchers {
  test("(2*x + 3)**2") {
    import PolynomialRationalTree.{parse, print, simplified, expanded}
    import PolynomialRationalTreeEvaluator.{value => polyValue}
    val e = simplified(expanded(parse("(2*x + 3)**2").get))
    print(e) shouldBe "4*x**2+6*x+6*x+9" // monomials are not simplified in expression trees
    polyValue(e) === Polynomial.dense(Array(Rational(9), Rational(12), Rational(4))) shouldBe true
  }
}
