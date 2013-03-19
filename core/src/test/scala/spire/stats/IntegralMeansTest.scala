package spire.stats

import org.scalatest.FunSuite

import spire.math.Integral
import spire.stats.integral._

class IntegralMeansTest extends FunSuite {
  def testArithmeticMean[M[_]: IntegralMeans,A: Integral](xs: M[A])(goal: A) = {
    assert(goal === xs.arithmeticMean)
  }

  test("arithmetic mean of empty array") {
    testArithmeticMean(Array[Int]())(0)
  }

  test("arithmetic mean singleton") {
    testArithmeticMean(Array(1))(1)
  }

  test("arithmetic mean of same values") {
    testArithmeticMean(Array.fill(5)(42))(42)
  }

  test("arithmetic mean of 1 to 5 array") {
    testArithmeticMean(Array.range(1,6))(3)
  }
}
