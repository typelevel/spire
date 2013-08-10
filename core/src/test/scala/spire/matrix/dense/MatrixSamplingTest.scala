package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.dense.random._
import org.scalatest.FunSuite

class MatrixSamplingTest extends FunSuite {

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("Test Matrix Samples") {
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
}