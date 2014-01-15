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
    for {
      (transA, transB, alpha, a, b, beta, c, m, n, k)
          <- elts.matrixProductSample
      cRef = c.copyToMatrix
      cFast = c.copyToMatrix
    } {
      referenceGemm(transA, transB, alpha, a, b, beta, cRef)
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
      cRef = Matrix.empty(m,n)
      cFast = Matrix.empty(m,n)
    } {
      referenceGemm(NoTranspose, NoTranspose, 1.0, a, b, 0.0, cRef)
      fastGemm(NoTranspose, NoTranspose, 1.0, a, b, 0.0, cFast)
      assert(cFast == cRef,
             s"[$m x $k] [$k x $n]: fast GEMM disagrees with reference GEMM")
    }
  }
}

class FastTrsmTest extends FunSuite {
  import BLAS.NaiveLevel3.{trsm => referenceTrsm}
  import BLAS.FastLevel3.{trsm => fastTrsm}
  //implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  implicit val gen = Defaults.IntegerGenerator.fromTime(1)

  val title = "Fast TRSM shall give the same results as reference TRSM"

  def diff(a:Matrix, b:Matrix) = {
    a.zip(b).map((xy:(Double, Double)) => {
      val (x,y) = xy
      val s = x.abs + y.abs
      if(s != 0) (x - y).abs/s else 0
    }).max
  }

  def msg(side:Sides.Value, uplo:UpperOrLower.Value,
          trans:Transposition.Value, diag:DiagonalProperty.Value,
          alpha:Double, m:Int, n:Int) = {
    val mat = (uplo, diag) match {
      case (Lower, NonUnitDiagonal) =>  """|[ x       ]
                                           |[ x x     ]
                                           |[ x x x   ]
                                           |[ x x x x ]
                                        """.stripMargin
      case (Lower, UnitDiagonal)    =>  """|[ 1       ]
                                           |[ x 1     ]
                                           |[ x x 1   ]
                                           |[ x x x 1 ]
                                        """.stripMargin
      case (Upper, NonUnitDiagonal) =>  """|[ x x x x ]
                                           |[   x x x ]
                                           |[     x x ]
                                           |[       x ]
                                        """.stripMargin
      case (Upper, UnitDiagonal)    =>  """|[ 1 x x x ]
                                           |[   1 x x ]
                                           |[     1 x ]
                                           |[       1 ]
                                        """.stripMargin
    }
    s"""|A${if(trans == Transpose) "^T" else ""} X = alpha B
        |A is $m x $m and shaped
        |$mat
        |X and B are $m x $n
        |alpha = $alpha
     """.stripMargin
  }

  test(s"$title (small matrices)") {
    val p2d = new ScalarUniformDistributionFromMinusOneToOne()
    val elts = new RandomUncorrelatedElements(nonSpecialScalars = 1,
                                              nonSpecialDimensions=1,
                                              scalars = p2d,
                                              elements = p2d)

    for {
      (side, uplo, trans, diag, alpha, a, b, m, n)
        <- elts.triangularSystemSample
      xRef = b.copyToMatrix
      xFast = b.copyToMatrix
      if side == FromLeft && uplo == Lower && trans == NoTranspose
    } {
      referenceTrsm(side, uplo, trans, diag, alpha, a, xRef)
      fastTrsm(side, uplo, trans, diag, alpha, a, xFast)
      assert(diff(xFast, xRef) < 0.001,
             s"""|Expected ${xRef.formatted("%.18f")}
                 |but got ${xFast.formatted("%.18f")} with
                 |${msg(side, uplo, trans, diag, alpha, m, n)}
                 |a=${a.formatted("%.8f", true)}
                 |b=${b.formatted("%.8f", true)}
                 |~~~
                 |""".stripMargin)
    }
  }

  test(s"$title (large matrices)") {
    import BLAS.FastLevel3._
    val blockingForGEBP = GEBP.threadLocalBlocking.get()
    val mc = blockingForGEBP.mc
    val blockingForTRSLowerBP = TRSLowerBP.threadLocalBlocking.get()
    val nc = blockingForTRSLowerBP.nc
    val p2d = new ScalarUniformDistributionFromMinusOneToOne()
    val elts = new RandomUncorrelatedElements(scalars = p2d,
                                              elements = p2d)
    for{
      (m,n) <- Iterator((mc+1, nc+1), (mc+mc/2, nc+nc/2))
      diag <- Iterator(NonUnitDiagonal, UnitDiagonal)
      a <- elts.triangularMatrixSample(m, Lower, diag).take(1)
      b <- elts.generalMatrixSample(m,n).take(1)
      xRef = b.copyToMatrix
      xFast = b.copyToMatrix
      side = FromLeft
      uplo = Lower
      trans = NoTranspose
      alpha = -1.0
    } {
      val (m,n) = b.dimensions
      referenceTrsm(side, uplo, trans, diag, alpha, a, xRef)
      fastTrsm(side, uplo, trans, diag, alpha, a, xFast)
      assert(diff(xFast, xRef) < 0.01,
             s"""|Fast TRSM disagrees with reference TRSM for
                 |${msg(side, uplo, trans, diag, alpha, m, n)}""".stripMargin)
    }
  }
}
