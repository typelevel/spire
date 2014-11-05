package spire.matrix.dense.tests

import spire.matrix.dense.BLAS
import spire.matrix.{Transposition, UpperOrLower, DiagonalProperty}
import Transposition._
import UpperOrLower._
import DiagonalProperty._
import spire.matrix.dense.{Matrix,Vector}
import spire.matrix.dense.random._

import org.scalatest.FunSuite

trait BLASLevel2Test extends FunSuite with BLAS.Level2 {

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
    assertResult(Vector(-26, -10, -17)) { y1 }
    val y2 = y.copyToVector
    gemv(NoTranspose, alpha=0, a, x, beta, y2)
    assertResult(Vector(-2, -2, -3)) { y2 }
    val y3 = y.copyToVector
    gemv(NoTranspose, alpha, a, x, beta=0, y3)
    assertResult(Vector(-24, -8, -14)) { y3 }
    val y4 = y.copyToVector
    gemv(NoTranspose, alpha, a, x, beta=1, y4)
    assertResult(Vector(-22, -6, -11)) { y4 }
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
    assertResult(Vector(5, -1, 8, 25, -27)) { y1 }
    val y2 = y.copyToVector
    gemv(Transpose, alpha=0, a, x, beta, y2)
    assertResult(Vector(3, -1, 2, -1, -3)) { y2 }
    val y3 = y.copyToVector
    gemv(Transpose, alpha, a, x, beta=0, y3)
    assertResult(Vector(2, 0, 6, 26, -24)) { y3 }
    val y4 = y.copyToVector
    gemv(Transpose, alpha, a, x, beta=1, y4)
    assertResult(Vector(-1, 1, 4, 27, -21)) { y4 }
  }

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  def trmvSample: Iterator[(UpperOrLower.Value, Transposition.Value,
                            DiagonalProperty.Value, Matrix, Vector, Vector)] = {
    val p2d = new ScalarUniformPowerOfTwoDistribution(minPower=0, maxPower=6)
    val testMatrices = new RandomUncorrelatedElements(
        scalars = p2d,
        elements = p2d)
    for {
      uplo <- Iterator(Upper, Lower)
      diag <- Iterator(UnitDiagonal, NonUnitDiagonal)
      trans <- Iterator(NoTranspose, Transpose)
      n <- testMatrices.oneDimensionSample
      a <- testMatrices.triangularMatrixSample(n, uplo, diag).take(1)
      x <- testMatrices.vectorSample(n).take(2)
      y = Vector.empty(x.dimension)
    } yield (uplo, trans, diag, a, x, y)
  }

  test("Triangular Matrix Vector product (TRMV)") {
    for((uplo, trans, diag, a, x, y) <- trmvSample) {
        gemv(trans, 1.0, a, x, 0.0, y)
        trmv(uplo, trans, diag, a, x)
        assert(x == y, (uplo, trans, diag, a, x))
    }
  }
}

class NaiveBLASLevel2Test extends BLASLevel2Test with BLAS.NaiveLevel2
