package spire.matrix.tests

import spire.matrix.dense._
import spire.matrix.BLAS
import spire.matrix.BLAS.Transposition._
import spire.matrix.NumericPropertiesOfDouble

import org.scalatest.FunSuite

trait HessenbergTestLike extends FunSuite
with NumericPropertiesOfDouble
with BLAS.level3.Naive
{
  val HessenbergDecomposition: Hessenberg.DecompositionLikeCompanion

  val eps = precision

  def normalityGoodness(q:MatrixLike) = {
    // TODO: use SYRK instead when we have code to model symmetric matrices
    val (m,n) = q.dimensions
    val d = Matrix.identity(n)
    gemm(Transpose, NoTranspose, -1.0, q, q, 1.0, d)
    d.norm1/(n*eps)
  }

  def decompositionGoodness(a:MatrixLike, q:MatrixLike, h:MatrixLike) = {
    val (m,n) = q.dimensions
    val qh = Matrix.empty(m, n)
    gemm(NoTranspose, NoTranspose, 1.0, q, h, 0.0, qh)
    val d = a.copyToMatrix
    gemm(NoTranspose, Transpose, -1.0, qh, q, 1.0, d)
    // TODO: check LAPACK code as I think it's a wee more complicated than that
    d.norm1/(a.norm1*m*eps)
  }

  test("Generic 3x3 matrix, with or without triangular padding") {
    // This is not a precision test, just a corner case test
    val m0 = Matrix(3,3)(100, 500, 700,
                         300,   0, 600,
                         400, 300, 100)
    val h = HessenbergDecomposition.withUnblockedAlgorithm(m0.copyToMatrix)
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
    val h1 = HessenbergDecomposition.withUnblockedAlgorithm(m1)
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

  test("Random 5x5 matrix") {
    val m = Matrix.empty(5,5)
    val generator = spire.random.Well512.fromTime(0)
    for(k <- 0 until m.length) m(k) = generator.nextDouble(-1.0, 1.0)
    val m0 = m.copyToMatrix
    val hd = HessenbergDecomposition.withUnblockedAlgorithm(m)
    val q = hd.transformationWithUnblockedAlgorithm
    val h = hd.reducedMatrix
    // TODO: check that value 20 is what LAPACK nep.in sets by default
    assert(normalityGoodness(q) < 20)
    assert(decompositionGoodness(m0, q, h) < 20)
  }
}

class HessenbergWithNaiveBLASTest extends HessenbergTestLike {
  val HessenbergDecomposition = Hessenberg.DecompositionWithNaiveBLAS
}
