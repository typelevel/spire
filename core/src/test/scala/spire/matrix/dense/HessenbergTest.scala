package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.Transposition._
import spire.matrix.UpperOrLower._
import spire.matrix.BLAS
import spire.matrix.dense.random._

import scala.math._
import org.scalatest.FunSuite

trait HessenbergTestLike extends FunSuite
with CommonMatrixPropertyTests
with BLAS.level3.Naive
{
  val HessenbergDecomposition: Hessenberg.DecompositionLikeCompanion

  def decompositionGoodness(a:MatrixLike, q:MatrixLike, h:MatrixLike) = {
    val (m,n) = q.dimensions
    val qh = Matrix.empty(m, n)
    gemm(NoTranspose, NoTranspose, 1.0, q, h, 0.0, qh)
    val d = a.copyToMatrix
    gemm(NoTranspose, Transpose, -1.0, qh, q, 1.0, d)
    // Reference: subroutine DHST01 in LAPACK
    val underflow = safeMinimum
    val aNorm = max(a.norm1, underflow)
    min(d.norm1, aNorm)/max(underflow*n/eps, a.norm1*eps)/n
  }

  test("Generic 3x3 matrix, with or without triangular padding") {
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(3)
      + HessenbergDecomposition.unblockedMinimumScratchpad(8))

    // This is not a precision test, just a corner case test
    val m0 = Matrix(3,3)(100, 500, 700,
                         300,   0, 600,
                         400, 300, 100)
    val h = HessenbergDecomposition.withUnblockedAlgorithm(m0.copyToMatrix)()
    expectResult(Matrix(3,3)( 100, -860,   20,
                             -500,  496,  -72,
                                0,  228, -396))(h.reducedMatrix)
    expectResult(Matrix(3,3)(1.0,  0.0 ,   0.0 ,
                             0.0, -0.60,  -0.80,
                             0.0, -0.80,   0.60)) {
      h.transformationWithUnblockedAlgorithm.round(9)
    }

    val m1 = Matrix.empty(8,8)
    m1.block(0,2)(0,2) := Matrix(2,2)(1, 2,
                                      0, 3)
    val a = Matrix(2,3)((10 to 60 by 10).map(-_.toDouble): _*)
    m1.block(0,2)(2,5) := a
    m1.block(0,2)(5,8) := a
    m1.block(2,5)(2,5) := m0
    m1.block(2,5)(5,8) := Matrix(3,3)((10 to 90 by 10).map(-_.toDouble): _*)
    m1.block(5,8)(5,8) := Matrix(3,3)(1, 2, 3,
                                      0, 4, 5,
                                      0, 0, 6)
    val h1 = HessenbergDecomposition.withUnblockedAlgorithm(m1)()
    val expected = Matrix(8,8)(
      1.0, 2.0, -10, 36, -2, -10, -20, -30,
      0.0, 3.0, -40, 78,  4, -40, -50, -60,
      0.0, 0.0, 100.0, -860.0, 20.0, -10, -20, -30,
      0.0, 0.0, -500.0, 496.0, -72.0, 80, 94, 108,
      0.0, 0.0, 0.0, 228.0, -396.0, -10, -8, -6,
      0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 5.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.0)
    expectResult(expected) { h1.reducedMatrix }
  }

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("Sample of test matrices ") {
    val eigenTests = new EigenTestMatrices
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(
        eigenTests.oneDimensionSample.max))
    for(n <- eigenTests.oneDimensionSample) {
      for((itype, a) <- eigenTests.sample(n)) {
        val a0 = a.copyToMatrix
        val hd = HessenbergDecomposition.withUnblockedAlgorithm(a)()
        val q = hd.transformationWithUnblockedAlgorithm
        val h = hd.reducedMatrix
        // That value 20 is what LAPACK nep.in sets by default
        assert(orthogonalityMeasure(q) < 20)
        assert(decompositionGoodness(a0, q, h) < 20, (itype, (a0, q, h)))
      }
    }
  }
}

class HessenbergWithNaiveBLASTest extends HessenbergTestLike {
  val HessenbergDecomposition = Hessenberg.DecompositionWithNaiveBLAS
}
