package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.dense.random._
import spire.std.any._
import spire.syntax.nroot._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable
import scala.math._

class MatrixSamplingTest extends FunSuite with ShouldMatchers {

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("General Matrix Samples") {
    val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
    val matrices = new TestGeneralMatrices(elements=uniformIm1p1)
    expectResult((1,1) :: (1,2) :: (1,3) :: (1,5) :: (1,10) :: (1,16) ::
                 (2,1) :: (2,2) :: (2,3) :: (2,5) :: (2,10) :: (2,16) ::
                 (3,1) :: (3,2) :: (3,3) :: (3,5) :: (3,10) :: (3,16) ::
                 (5,1) :: (5,2) :: (5,3) :: (5,5) :: (5,10) :: (5,16) ::
                 (10,1) :: (10,2) :: (10,3) :: (10,5) :: (10,10) :: (10,16) ::
                 (16,1) :: (16,2) :: (16,3) :: (16,5) :: (16,10) :: (16,16) ::
                 Nil) {
      matrices.generalMatrixSample.map(_.dimensions).toList
    }

    expectResult(List.fill(36)(true)) {
      matrices.generalMatrixSample.map(_.forall(e => -1 <= e && e <= 1)).toList
    }
  }

  test("Tailored Eigenvalues: default settings") {
    // Sanity checks, not precision checks!
    val eigenTests = new EigenTestMatrices
    val ulp = eigenTests.ulp
    val n = 8
    val sqrtOverflowThreshold = eigenTests.sqrtOverflowThreshold(n)
    val sqrtUnderflowThreshold = eigenTests.sqrtUnderflowThreshold(n)
    val types = mutable.ListBuffer.empty[Int]
    def sumArithmetic(n:Int) = 0.5*n*(1 + ulp)
    for((itype, a) <- eigenTests.sample(n)) {
      types += itype
      assert(a.isSquare)
      val n = a.dimensions._1
      itype match {
        case 1 => { a should be ('zero) }
        case 2 => { a should be ('identity) }
        case 3 => {} // todo
        case 4 => {
          a should be ('diagonal)
          a.diagonal.map(abs).sum should be (
            sumArithmetic(n) plusOrMinus 5e-16)
        }
        case 5 => {
          a should be ('diagonal)
          abs(a.diagonal.product) should be (
            ulp**(0.5*n) plusOrMinus 5e-75)
        }
        case 6 => {
          a should be ('diagonal)
          abs(a.diagonal.map(abs).sum) should be (
            1 + (n-1)*ulp plusOrMinus 5e-16)
        }
        case 7 | 8 => {
          (a should be ('diagonal))
          val diag = a.diagonal
          abs(diag(n-1)/diag(0)) should be === ulp
          if(itype == 7) abs(diag(0)) should be === sqrtOverflowThreshold
          else abs(diag(0)) should be === sqrtUnderflowThreshold
        }
        case 19 => {
          a should not be ('diagonal)
          a.forall(e => -1 <= e && e < 1) should be === true
        }
        case 20 => {
          a should not be ('diagonal)
          a.map(e => if     (e < -sqrtOverflowThreshold) -1
                     else if(e >  sqrtOverflowThreshold)  1
                     else                                 0
          ) === List.fill(n*n)(0)
        }
        case 21 => {
          a should not be ('diagonal)
          a.forall(e => -sqrtUnderflowThreshold <= e
                          && e <= sqrtUnderflowThreshold) should be === true
        }
        case _ => {
          // To do better we would need at least a way to estimate condition
          // number so as to distinguish orthogonal similarities from
          // ill-conditioned ones
          a should not be ('diagonal)
        }
      }
    }
    types.toList should be === (1 to 21).toList
  }

  test("Corner cases") {
    val eigTests = new EigenTestMatrices(includedTypes=Set(4))
    assert(List(Matrix(1,1)(1), Matrix(1,1)(-1)).contains(
                                                eigTests.sample(1).head._2))
  }
}