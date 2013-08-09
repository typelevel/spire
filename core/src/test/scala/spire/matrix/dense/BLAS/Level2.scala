package spire.matrix.BLAS.level2.tests

import spire.matrix.BLAS
import BLAS.Transposition._
import spire.matrix.dense.{Matrix,Vector}

import org.scalatest.FunSuite

trait BLASLevel2Test extends FunSuite with BLAS.level2.Interface {

  test("General Matrix Vector product (GEMV): no transpose") {
    val a = Matrix(3,5)( 2, -2, 1, -1,  2,
                         3, -1, 2,  3, -2,
                         4,  1, 1, -2,  2)
    val x = Vector(-1, 3, -2, 2, 0)
    val y = Vector(2, 2, 3)
    val y1 = y.copyToVector
    val alpha = 2
    val beta = -1
    gemv(NoTranspose, alpha, a, x, beta, y1)
    expectResult(Vector(-26, -10, -17)) { y1 }
    val y2 = y.copyToVector
    gemv(NoTranspose, alpha=0, a, x, beta, y2)
    expectResult(Vector(-2, -2, -3)) { y2 }
    val y3 = y.copyToVector
    gemv(NoTranspose, alpha, a, x, beta=0, y3)
    expectResult(Vector(-24, -8, -14)) { y3 }
    val y4 = y.copyToVector
    gemv(NoTranspose, alpha, a, x, beta=1, y4)
    expectResult(Vector(-22, -6, -11)) { y4 }
  }

  test("General Matrix Vector product (GEMV): transpose") {
    val a = Matrix(3,5)( 2, -2, 1, -1,  2,
                         3, -1, 2,  3, -2,
                         4,  1, 1, -2,  2)
    val x = Vector(-2, 3, -1)
    val y = Vector(-3, 1, -2, 1, 3)
    val y1 = y.copyToVector
    val alpha = 2
    val beta = -1
    gemv(Transpose, alpha, a, x, beta, y1)
    expectResult(Vector(5, -1, 8, 25, -27)) { y1 }
    val y2 = y.copyToVector
    gemv(Transpose, alpha=0, a, x, beta, y2)
    expectResult(Vector(3, -1, 2, -1, -3)) { y2 }
    val y3 = y.copyToVector
    gemv(Transpose, alpha, a, x, beta=0, y3)
    expectResult(Vector(2, 0, 6, 26, -24)) { y3 }
    val y4 = y.copyToVector
    gemv(Transpose, alpha, a, x, beta=1, y4)
    expectResult(Vector(-1, 1, 4, 27, -21)) { y4 }
  }
}

class NaiveBLASLevel2Test extends BLASLevel2Test with BLAS.level2.Naive
