package spire.matrix.dense.tests

import spire.matrix.dense._

import org.scalatest.FunSuite
import scala.math.sqrt

class EuclideanNormTest extends FunSuite with EuclideanNorm {

  val m = Matrix(5,3)( 1, 12,    1,
                       2,  8,    1e-8,
                       3, -7,    2,
                       4,  4,   -1,
                      -5,  2,    1e-7
                      )

  test("Trivial cases") {
    val x1 = new  MatrixStrides(m, first=(4,0), step=1, length=1)
    expectResult(5.0) { euclideanNorm(x1) }
    val x2 = new  MatrixStrides(m, first=(0,0), step=1, length=0)
    expectResult(0.0) { euclideanNorm(x2) }
  }

  test("Decreasing") {
    val x1 = m.column(1)
    expectResult(sqrt(277.0)) { euclideanNorm(x1) }
  }

  test("Increasing") {
    val x1 = m.column(0)
    expectResult(sqrt(55.0)) { euclideanNorm(x1) }
  }

  test("Badly scaled") {
    val x1 = m.column(2)
    expectResult(2.4494897427831801599) { euclideanNorm(x1) }
  }

  test("Dimension 2") {
    expectResult(5.0) { euclideanNorm2D(4, -3) }
    expectResult(4.0000000000000000125) { euclideanNorm2D(4, 1e-8) }
  }
}