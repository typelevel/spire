package spire.matrix.dense.tests

import spire.matrix.dense.BLAS
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Transposition._
import UpperOrLower._
import Sides._
import DiagonalProperty._
import spire.matrix.dense.Matrix
import spire.matrix.dense.random._


import org.scalatest.FunSuite

trait BLASLevel3Test extends FunSuite with BLAS.Level3 {

  test("General Matrix Multiplication (GEMM)") {
    val a = Matrix(5,3)(-2, -1, -3,
                         1, -3,  3,
                         1,  0,  5,
                         4, -2, -2,
                        -5, -3, -4)
    val b = Matrix(3,4)( 5,  0,  2, -5,
                        -4, -4, -5,  5,
                        -5,  2, -3,  4)
    val c = Matrix(5,4)(-1,  5,  5,  5,
                        -3, -4,  4,  0,
                        -3, -4,  5,  4,
                         1,  1, -5, -3,
                         2,  0, -4, -1)

    for(transA <- NoTranspose :: Transpose :: Nil;
        transB <- NoTranspose :: Transpose :: Nil) {
      val a1 = if(transA == Transpose) a.transposed else a
      val b1 = if(transB == Transpose) b.transposed else b
      val c1 = c.copyToMatrix
      gemm(transA = transA, transB = transB,
           alpha = 0, a = a1, b = b1, beta = 0, c = c1)
      expectResult(Matrix.zero(5,4)){ c1 }

      val c2 = c.copyToMatrix
      gemm(transA = transA, transB = transB,
           alpha = 0, a = a1, b = b1, beta = 1, c = c2)
      expectResult(c){ c2 }

      val c3 = c.copyToMatrix
      gemm(transA = transA, transB = transB,
           alpha = 0, a = a1, b = b1, beta = 2, c = c3)
      expectResult(Matrix(5,4)(-2, 10, 10, 10,  -6,
                               -8,  8,  0, -6,  -8,
                               10,  8,  2,  2, -10,
                               -6,  4,  0, -8,  -2)) { c3 }
    }

    val expectedC = Matrix(5,4)(-27,   6, -30,  21,
                                 -6, -54, -24,  24,
                                 60, -30,  39, -45,
                               -114, -12, -72, 114,
                                -21, -12, -51,  18)

    val c1 = c.copyToMatrix
    gemm(transA = NoTranspose, transB = NoTranspose,
         alpha = -3, a = a, b = b, beta = 0, c = c1)
    expectResult(expectedC) { c1 }

    val c2 = c.copyToMatrix
    gemm(transA = Transpose, transB = NoTranspose,
         alpha = -3, a = a.transposed, b = b, beta = 0, c = c2)
    expectResult(expectedC) { c2 }

    val c3 = c.copyToMatrix
    gemm(transA = NoTranspose, transB = Transpose,
         alpha = -3, a = a, b = b.transposed, beta = 0, c = c3)
    expectResult(expectedC) { c3 }

    val c4 = c.copyToMatrix
    gemm(transA = Transpose, transB = Transpose,
         alpha = -3, a = a.transposed, b = b.transposed, beta = 0, c = c4)
    expectResult(expectedC) { c4 }
  }

  test("GEMM: matrix x block") {
    val (m,n) = (10,7)
    val b = Matrix(m,n)( 4, -4,  1,  1, -4,  2, -1,
                          5,  2,  0, -2,  0,  3, -5,
                         -2,  5,  5, -4, -5, -4,  2,
                         -5,  1,  5,  2,  2,  2, -5,
                          1,  4, -4,  2, -4,  0, -2,
                          5,  4,  2,  2, -5, -4,  0,
                          4, -5, -5, -3, -3, -2, -5,
                          3,  1, -4,  5, -5, -3,  2,
                         -2, -4, -1, -2,  2,  0, -4,
                          5,  3, -2, -4, -3,  0,  3)
    val a = Matrix(3,3)(-2, -1,  3,
                        -1, -3,  0,
                         2, -2, -2)
    gemm(transA = NoTranspose, transB = NoTranspose,
         alpha = -2, a = a, b = b.block(7, m)(2, n),
         beta = 1, c = b.block(0,3)(0,5))
    val expectedB = Matrix(m,n)(-2,  36,  3, -11, -22,  2, -1,
                                 -9,   0,  2,  -8, -20,  3, -5,
                                  2, -39, 21,   8, -17, -4,  2,
                                 -5,   1,  5,   2,   2,  2, -5,
                                  1,   4, -4,   2,  -4,  0, -2,
                                  5,   4,  2,   2,  -5, -4,  0,
                                  4,  -5, -5,  -3,  -3, -2, -5,
                                  3,   1, -4,   5,  -5, -3,  2,
                                 -2,  -4, -1,  -2,   2,  0, -4,
                                  5,   3, -2,  -4,  -3,  0,  3)
    expectResult(expectedB) { b }
  }

  test("Symmetric Rank-k Update (SYRK)") {
    val c = Matrix(5,5)(-2, -3, 2, -1, 0,
                        3, 0, 1, -2, -3,
                        0, 3, 3, 2, -2,
                        1, 1, -1, 0, 3,
                        2, -2, -3, 3, 0)
    val a1 = Matrix(5,9)(2, 2, -4, -1, 0, 2, 0, -2, -3,
                         -2, 0, -1, -4, -4, -1, -1, -4, 4,
                         2, -1, -4, 1, -1, 1, 4, -4, -2,
                         -2, 3, -1, 3, 2, -1, -2, -4, 3,
                         1, -2, 3, 3, -3, 0, 3, 1, -3)
    val a2 = Matrix(9,5)(-2, 4, 2, 2, 3,
                         2, 1, 1, -3, 4,
                         0, -1, 1, 1, -1,
                         2, 1, -3, -3, 0,
                         1, 2, -3, 4, 0,
                         -2, -1, -4, 3, -4,
                         -1, -2, -1, 2, -4,
                         3, 1, -4, -3, -1,
                         2, 4, -2, 4, 2)

    val euc1 = Matrix(5,5)(-46, -4, -29, -2, 10,
                          3, -71, -1, -20, 18,
                          0, 3, -54, 5, -16,
                          1, 1, -1, -57, 33,
                          2, -2, -3, 3, -51)
    val euc2 = Matrix(5,5)(-35, -19, 22, 19, -15,
                          3, -45, 9, -19, -42,
                          0, 3, -55, 15, -33,
                          1, 1, -1, -77, 22,
                          2, -2, -3, 3, -63)
    val uc1 = c.copyToMatrix
    syrk(uplo = Upper, trans = NoTranspose,
         alpha = -1, a = a1, beta = 2, c = uc1)
    expectResult(euc1) { uc1 }
    val uc2 = c.copyToMatrix
    syrk(uplo = Upper, trans = Transpose,
         alpha = -1, a = a2, beta = 2, c = uc2)
    expectResult(euc2) { uc2 }

    val elc1 = Matrix(5,5)(-46, -3, 2, -1, 0,
                           8, -71, 1, -2, -3,
                           -33, 3, -54, 2, -2,
                           2, -14, -1, -57, 3,
                           14, 20, -18, 33, -51)
    val lc1 = c.copyToMatrix
    syrk(uplo = Lower, trans = NoTranspose,
         alpha = -1, a = a1, beta = 2, c = lc1)
    expectResult(elc1) { lc1 }

    val elc2 = Matrix(5,5)(-35, -3, 2, -1, 0,
                           -7, -45, 1, -2, -3,
                           18, 13, -55, 2, -2,
                           23, -13, 9, -77, 3,
                           -11, -40, -35, 22, -63)
    val lc2 = c.copyToMatrix
    syrk(uplo = Lower, trans = Transpose,
         alpha = -1, a = a2, beta = 2, c = lc2)
    expectResult(elc2) { lc2 }
  }

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  def syrkSample(trans:Transposition.Value)
  : Iterator[(Double, Matrix, Double, Matrix)] = {
    val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
    val g = new RandomUncorrelatedElements(
      nonSpecialDimensions = 4,
      nonSpecialScalars = 1,
      scalars = uniformIm1p1,
      elements = uniformIm1p1)
    for {
      (m,k) <- g.twoDimensionSample
      a <- (if(trans == NoTranspose) g.generalMatrixSample(m,k)
            else g.generalMatrixSample(k,m)).take(3)
      c <- g.generalMatrixSample(m,m).take(2)
      alpha <- g.scalarSample
      beta <- g.scalarSample
    } yield (alpha, a, beta, c)
  }

  def gemmVsSyrkSample(trans:Transposition.Value) =
    for {
      (alpha, a, beta, c) <- syrkSample(trans)
      cGemm = c.copyToMatrix
      cSyrk = c.copyToMatrix
      } yield (alpha, a, beta, cGemm, cSyrk)

  test("SYRK only touches upper or lower triangle") {
    for((alpha, a, beta, c) <- syrkSample(Transpose)) {
      val cSyrk = c.copyToMatrix
      syrk(Lower, Transpose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToUpperDiagonal(1) === c.copyToUpperDiagonal(1))
    }
    for((alpha, a, beta, c) <- syrkSample(NoTranspose)) {
      val cSyrk = c.copyToMatrix
      syrk(Lower, NoTranspose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToUpperDiagonal(1) === c.copyToUpperDiagonal(1))
    }
    for((alpha, a, beta, c) <- syrkSample(Transpose)) {
      val cSyrk = c.copyToMatrix
      syrk(Upper, Transpose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToLowerDiagonal(-1) === c.copyToLowerDiagonal(-1))
    }
    for((alpha, a, beta, c) <- syrkSample(NoTranspose)) {
      val cSyrk = c.copyToMatrix
      syrk(Upper, NoTranspose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToLowerDiagonal(-1) === c.copyToLowerDiagonal(-1))
    }
  }

  test("GEMM compatibility with SYRK") {
    // C := alpha A^T A + beta C (lower triangle)
    for((alpha, a, beta, cGemm, cSyrk) <- gemmVsSyrkSample(Transpose)) {
      gemm(Transpose, NoTranspose, alpha, a, a, beta, cGemm)
      syrk(Lower, Transpose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToLowerDiagonal() === cGemm.copyToLowerDiagonal())
    }

    // C := alpha A^T A + beta C (upper triangle)
    for((alpha, a, beta, cGemm, cSyrk) <- gemmVsSyrkSample(Transpose)) {
      gemm(Transpose, NoTranspose, alpha, a, a, beta, cGemm)
      syrk(Upper, Transpose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToUpperDiagonal() === cGemm.copyToUpperDiagonal())
    }

    // C := alpha A A^T + beta C (lower triangle)
    for((alpha, a, beta, cGemm, cSyrk) <- gemmVsSyrkSample(NoTranspose)) {
      gemm(NoTranspose, Transpose, alpha, a, a, beta, cGemm)
      syrk(Lower, NoTranspose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToLowerDiagonal() === cGemm.copyToLowerDiagonal())
    }

    // C := alpha A A^T + beta C (upper triangle)
    for((alpha, a, beta, cGemm, cSyrk) <- gemmVsSyrkSample(NoTranspose)) {
      gemm(NoTranspose, Transpose, alpha, a, a, beta, cGemm)
      syrk(Upper, NoTranspose, alpha, a, beta, cSyrk)
      assert(cSyrk.copyToUpperDiagonal() === cGemm.copyToUpperDiagonal())
    }
  }

  def trsmSample: Iterator[(Sides.Value, UpperOrLower.Value, Transposition.Value,
                            DiagonalProperty.Value, Double, Matrix, Matrix,
                            Matrix)] = {
    val p2d = new ScalarUniformPowerOfTwoDistribution(minPower=0, maxPower=6)
    val testMatrices = new RandomUncorrelatedElements(scalars = p2d,
                                               elements = p2d)
    for {
      (m,n) <- testMatrices.twoDimensionSample
      x <- testMatrices.generalMatrixSample(m,n).take(1)
      b = Matrix.empty(m,n)
      uplo <- Iterator(Upper, Lower)
      diag <- Iterator(UnitDiagonal, NonUnitDiagonal)
      side <- Iterator(fromLeft, fromRight)
      a <- testMatrices.triangularMatrixSample(if(side == fromLeft) m else n,
                                               uplo, diag).take(1)
      trans <- Iterator(NoTranspose, Transpose)
      alpha <- testMatrices.scalarSample
    } yield {
      if(side == fromLeft)
        gemm(trans, NoTranspose, 1/alpha, a, x, 0.0, b)
      else
        gemm(NoTranspose, trans, 1/alpha, x, a, 0.0, b)
      (side, uplo, trans, diag, alpha, a, b, x)
    }
  }

  test("Triangular Solver (TRSM)") {
    for ((side, uplo, trans, diag, alpha, a, b, x0) <- trsmSample) {
      trsm(side, uplo, trans, diag, alpha, a, b)
      val x = b
      val msg = s"""|Expected x=$x but got x=$b
                    |for side=$side, uplo=$uplo, trans=$trans,
                    |    diag=$diag, alpha=$alpha, and
                    | a=$a
                    | b=$b""".stripMargin
      if(alpha == 0) assert(x.isZero, msg)
      else assert(b == x, msg)
    }
  }
}

class NaiveBLASLevel3Test extends BLASLevel3Test with BLAS.NaiveLevel3
