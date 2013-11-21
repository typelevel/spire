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

  // from LAPACK dtest.in
  val threshold = 30.0
  val rightHandSides = Seq(1, 2, 15)

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

  def residualForSolution(a:Matrix, b:Matrix, x:Matrix) = {
    val normA = a.norm1
    if(normA == 0) 1.0/eps
    else {
      // b := b - A X
      gemm(NoTranspose, NoTranspose, -1.0, a, x, 1.0, b)

      val (m, nRhs) = b.dimensions
      (for(j <- 0 until nRhs) yield {
        val normB = b.column(j).map(_.abs).sum
        val normX = x.column(j).map(_.abs).sum
        if(normX == 0) 1.0/eps
        else ((normB/normA)/normX)/eps
        }).max
    }
  }

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("LU decomposition") {
    val linTests = new LinearSystemTestMatrices(nonSpecialDimensions = 1)
    val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
    val elts = new RandomUncorrelatedElements(elements = uniformIm1p1)
    info(elts.oneDimensionSample.toList.toString)
    for((imat, zeroIdx, a) <- linTests.sample) {
      val a0 = a.copyToMatrix
      val (m,n) = a.dimensions

      info(s"Type $imat, dimension $m x $n")
      try {
        info("\t+ Decomposition P A = L U")
        val lu = TestedLUDecompositionConstruction(a)
        val goodnessLU = decompositionGoodness(a0, lu)
        assert(goodnessLU < threshold,
               s"""|Failure: $goodnessLU >= $threshold
                   |A=$a0
                   |$lu
                   |""".stripMargin)

        if(m == n && m > 1) {
          info("\t+ Solution of A X = B")
          for(nRhs <- rightHandSides;
              x <- elts.generalMatrixSample(m, nRhs).take(1)) {
            // Construct rhs B = A X
            val b = Matrix.empty(m, nRhs)
            gemm(NoTranspose, NoTranspose, 1.0, a, x, 0.0, b)

            // Solve A X = B and test residual
            val b0 = b.copyToMatrix
            lu.solve(NoTranspose, b)
            val residual = residualForSolution(a0, b0, b)
            assert(residual < threshold,
                   s"""|Failure $residual >= $threshold in solving A X = B
                       |A=$a0
                       |B=$b0""".stripMargin)
          }
        }
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

