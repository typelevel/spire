package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.NumericPropertiesOfDouble
import spire.matrix.Transposition._
import spire.matrix.UpperOrLower._
import spire.matrix.DiagonalProperty._
import spire.matrix.dense.BLAS
import spire.matrix.dense.random._
import spire.syntax.cfor._

import scala.math._
import org.scalatest.FunSuite

trait LUDecompositionTest extends FunSuite
with CommonMatrixPropertyTests
with BLAS.NaiveLevel1
with BLAS.NaiveLevel2
with BLAS.NaiveLevel3
{
  override val eps = epsilonMachine

  val TestedLUDecompositionConstruction: LU.DecompositionConstruction

  val threshold = 30.0 // from LAPACK dtest.in

  def decompositionGoodness(a:MatrixLike, lu:LU.Decomposition) = {
    val (m, n) = a.dimensions
    // residual starts as PLU
    val residual = lu.reconstructedOriginal

    // overwrite residual with PLU - A
    axpy(-1.0, a, residual)

    // properly normalised ratio of the norms of A and of the residual
    val normA = a.norm1
    val normResidual = residual.norm1
    if(normA <= 0) if(normResidual != 0) 1/eps else 0
    else ((normResidual/n)/normA)/eps
  }

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("LU decomposition") {
    val linTests = new LinearSystemTestMatrices
    for((imat, zeroIdx, a) <- linTests.sample) {
      val (m,n) = a.dimensions
      val a0 = a.copyToMatrix

      info(s"Type $imat, dimension $m x $n")
      try {
        val lu = TestedLUDecompositionConstruction(a)
        val goodnessLU = decompositionGoodness(a0, lu)
        assert(goodnessLU < threshold,
               s"""|Failure: $goodnessLU >= $threshold
                   |A=$a0
                   |$lu
                   |""".stripMargin)
      }
      catch {
        case ex:LU.Singularity =>
          assert(zeroIdx != None,
                 s"""|Unexpected singularity for matrix type #imat:
                     |$a0
                     |""".stripMargin)
          expectResult { zeroIdx.get } { ex.pivotIndex }
      }
    }
  }
}

class RecursiveLUDecompositionWithNaiveBLASTest extends LUDecompositionTest {
  val TestedLUDecompositionConstruction =
    LU.RecursiveDecompositionConstructionWithNaiveBLAS
}

