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

class FastGemmTest extends FunSuite {
  import BLAS.NaiveLevel3.{gemm => referenceGemm}
  import BLAS.FastLevel3.{gemm => fastGemm}
  implicit val gen = Defaults.IntegerGenerator.fromTime(1)

  test("Fast GEMM shall give the same results as reference GEMM") {
    val p2d = new ScalarUniformPowerOfTwoDistribution(minPower=0, maxPower=6)
    val elts = new RandomUncorrelatedElements(nonSpecialScalars = 1,
                                              scalars = p2d,
                                              elements = p2d)

    val blocking = BLAS.FastLevel3Blocking()

    def pretty(trans:Transposition.Value, r:Int, s:Int) =
      s"[$r x $s]${if(trans == Transpose) "^T" else ""}"
    def msg(transA:Transposition.Value, transB:Transposition.Value,
            alpha:Double, beta:Double, m:Int, n:Int, k:Int) =
      s"""|${pretty(transA, m, k)} x ${pretty(transB, k, n)}
          |alpha=$alpha
          |beta=$beta""".stripMargin

    for((transA, transB, alpha, a, b, beta, m, n, k)
          <- elts.matrixProductSample) {
      val cRef = Matrix.empty(m,n)
      referenceGemm(transA, transB, alpha, a, b, beta, cRef)
      val cFast = Matrix.empty(m,n)
      fastGemm(transA, transB, alpha, a, b, beta, cFast)
      assert(cFast == cRef,
             s"""|Expected $cRef but got $cFast with
                 |${msg(transA, transB, alpha, beta, m, n, k)}
                 |""".stripMargin)
    }
  }
}
