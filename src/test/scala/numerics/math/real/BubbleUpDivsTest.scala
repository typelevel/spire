package test.scala.numerics.math.real

import numerics.math._
import Implicits._

import org.scalatest.FunSuite


class BubbleUpDivsTest extends FunSuite {
  def hasRootDiv(a: Real) {
    assert(a match {
        case Div(_, _) => true
        case _ => false
      }, "Root Real expr is not a Div")
  }

  def hasOneDiv(a: Real) {
    assert(countDivs(a) == 1, "Real has more than 1 Div in expr")
  }

  def countDivs(a: Real): Int = a match {
    case Div(a, b) => 1 + countDivs(a) + countDivs(b)
    case BinOp(a, b) => countDivs(a) + countDivs(b)
    case Neg(a) => countDivs(a)
    case KRoot(a, _) => countDivs(a)
    case IntLit(_) => 0
    case BigIntLit(_) => 0
  }

  def assertDivBubbledUp(a: Real) {
    hasOneDiv(a)
    hasRootDiv(a)
  }

  test("Repeated divs result in 1 div") {
    val a = Iterator.fill(17)(Real(2)) reduce (_ / _)
    assertDivBubbledUp(a)
  }

  test("Adding divs results in 1 root div") {
    val a = Real(1) / 3 + Real(29) / 17
    assertDivBubbledUp(a)
  }

  test("Subtracting divs results 1 in root div") {
    val a = Real(1) / 3 - Real(29) / 17
    assertDivBubbledUp(a)
  }

  test("Multiplying divs results in 1 root div") {
    val a = Real(1) / 3 + Real(29) / 17
    assertDivBubbledUp(a)
  }

  test("KRoots of divs results in 1 root div") {
    val a = (Real(1) / 2).sqrt
    assertDivBubbledUp(a)
  }

  test("Negation results in 1 root div") {
    val a = -(Real(1) / 2)
    assertDivBubbledUp(a)
  }

  test("Mixed expression results in 1 div") {
    val a = (Real(2) / 3 + Real(1) / 3 + Real(2).sqrt * 3).sqrt
    assertDivBubbledUp(a)
  }
}
 

