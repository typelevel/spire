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
  import BLAS.FastLevel3.{gemm => fastGemm, GEBP}

  //implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  implicit val gen = Defaults.IntegerGenerator.fromTime(1)

  val p2d = new ScalarUniformPowerOfTwoDistribution(minPower=0, maxPower=6)
  val elts = new RandomUncorrelatedElements(nonSpecialScalars = 1,
                                            nonSpecialDimensions=1,
                                            scalars = p2d,
                                            elements = p2d)

  def pretty(trans:Transposition.Value, r:Int, s:Int) =
    s"[$r x $s]${if(trans == Transpose) "^T" else ""}"

  def msg(transA:Transposition.Value, transB:Transposition.Value,
          alpha:Double, beta:Double, m:Int, n:Int, k:Int) =
    s"""|${pretty(transA, m, k)} x ${pretty(transB, k, n)}
        |alpha=$alpha
        |beta=$beta""".stripMargin

  val title = "Fast GEMM shall give the same results as reference GEMM"

  test(s"$title (small matrices") {
    for((transA, transB, alpha, a, b, beta, c, m, n, k)
          <- elts.matrixProductSample) {
      val cRef = c.copyToMatrix
      referenceGemm(transA, transB, alpha, a, b, beta, cRef)
      val cFast = c.copyToMatrix
      fastGemm(transA, transB, alpha, a, b, beta, cFast)
      assert(cFast == cRef,
             s"""|Expected $cRef but got $cFast with
                 |${msg(transA, transB, alpha, beta, m, n, k)}
                 |""".stripMargin)
    }
  }

  test(s"$title (large matrices)") {
    val blocking = GEBP.threadLocalBlocking.get
    val mc = blocking.mc
    val kc = blocking.kc
    for{
      (m,k,n) <- Iterator((mc+1, kc+1, 5), (mc+mc/2, kc+kc/2, 5))
      a <- elts.generalMatrixSample(m,k).take(1)
      b <- elts.generalMatrixSample(k,n).take(1)
    } {
      val cRef = Matrix.empty(m,n)
      referenceGemm(NoTranspose, NoTranspose, 1.0, a, b, 0.0, cRef)
      val cFast = Matrix.empty(m,n)
      fastGemm(NoTranspose, NoTranspose, 1.0, a, b, 0.0, cFast)
      assert(cFast == cRef,
             s"[$m x $k] [$k x $n]: fast GEMM disagrees with reference GEMM")
    }
  }
}

}
