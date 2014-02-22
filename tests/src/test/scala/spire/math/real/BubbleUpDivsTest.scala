package spire.math.algebraic

import spire.math._
import spire.implicits._

import org.scalatest.FunSuite


class BubbleUpDivsTest extends FunSuite {
  def hasRootDiv(a: Algebraic) {
    assert(a match {
        case Div(_, _) => true
        case _ => false
      }, "Root Algebraic expr is not a Div")
  }

  def hasOneDiv(a: Algebraic) {
    assert(countDivs(a) == 1, "Algebraic has more than 1 Div in expr")
  }

  def countDivs(a: Algebraic): Int = a match {
    case Div(a, b) => 1 + countDivs(a) + countDivs(b)
    case Add(a, b) => countDivs(a) + countDivs(b)
    case Sub(a, b) => countDivs(a) + countDivs(b)
    case Mul(a, b) => countDivs(a) + countDivs(b)
    case Neg(a) => countDivs(a)
    case KRoot(a, _) => countDivs(a)
    case IntLit(_) => 0
    case BigIntLit(_) => 0
  }

  def assertDivBubbledUp(a: Algebraic) {
    hasOneDiv(a)
    hasRootDiv(a)
  }

  test("Repeated divs result in 1 div") {
    val a = Iterator.fill(17)(Algebraic(2)) reduce (_ / _)
    assertDivBubbledUp(a)
  }

  test("Adding divs results in 1 root div") {
    val a = Algebraic(1) / 3 + Algebraic(29) / 17
    assertDivBubbledUp(a)
  }

  test("Subtracting divs results 1 in root div") {
    val a = Algebraic(1) / 3 - Algebraic(29) / 17
    assertDivBubbledUp(a)
  }

  test("Multiplying divs results in 1 root div") {
    val a = Algebraic(1) / 3 + Algebraic(29) / 17
    assertDivBubbledUp(a)
  }

  test("KRoots of divs results in 1 root div") {
    val a = (Algebraic(1) / 2).sqrt
    assertDivBubbledUp(a)
  }

  test("Negation results in 1 root div") {
    val a = -(Algebraic(1) / 2)
    assertDivBubbledUp(a)
  }

  test("Mixed expression results in 1 div") {
    val a = (Algebraic(2) / 3 + Algebraic(1) / 3 + Algebraic(2).sqrt * 3).sqrt
    assertDivBubbledUp(a)
  }
}
 

