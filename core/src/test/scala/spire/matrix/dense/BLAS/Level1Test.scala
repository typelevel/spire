package spire.matrix.BLAS.level1.FunSuite

import spire.matrix.BLAS
import spire.matrix.dense._

import org.scalatest.FunSuite

trait BLASLevel1Test extends FunSuite with BLAS.level1.Interface {

  test("Vector scaling") {
    val m = Matrix(2,4)(1, 2, 3, 4,
                        5, 6, 7, 8)
    scale(3.0, m.column(0))
    scale(-2.0, m.row(1).block(0,3))
    expectResult(Matrix(2,4)(  3,   2,   3, 4,
                             -30, -12, -14, 8)) { m }
  }

}

class NaiveBLASLevel1Test extends BLASLevel1Test with BLAS.level1.Naive
