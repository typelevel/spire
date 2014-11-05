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

class LUReconstructionTest extends FunSuite {

  class LUDecomposition(val lu:Matrix, val p:Permutation)
  extends LU.Decomposition
  with BLAS.NaiveLevel3 with BLAS.NaiveLevel2 with BLAS.NaiveLevel1

  test("2 x 2") {
    val lu = Matrix(2,2)(2, 3,
                         4, 5)
    val lud = new LUDecomposition(lu, Permutation(1,1))
    assertResult { Matrix(2,2)(8, 17,
                               2,  3) } { lud.reconstructedOriginal }
  }

  test("3 x 3") {
    val lu = Matrix(3,3)( 2,  3, -1,
                         -1,  5, -2,
                          2, -3,  4)
    val lud = new LUDecomposition(lu, Permutation.identity(3))
    assertResult { Matrix(3,3)( 2,  3, -1,
                               -2,  2, -1,
                                4, -9,  8) } { lud.reconstructedOriginal }
    val lud1 = new LUDecomposition(lu, Permutation(1,2,2))
    assertResult { Matrix(3,3)( 4, -9,  8,
                                2,  3, -1,
                               -2,  2, -1) } { lud1.reconstructedOriginal }
  }

  test("4 x 3") {
    val lu = Matrix(4,3)(2, 4, 3,
                         4, 2, 3,
                         1, 3, 1,
                         3, 2, 4)
    val lud = new LUDecomposition(lu, Permutation(3,1,2,3))
    assertResult { Matrix(4,3)(6, 16, 19,
                               8, 18, 15,
                               2, 10, 13,
                               2,  4,  3) } { lud.reconstructedOriginal }
  }

  test("3 x 5") {
    val lu = Matrix(3,5)(2, 4, 2,
                         3, 4, 1,
                         2, 4, 1,
                         4, 4, 2,
                         4, 4, 2)
    val lud = new LUDecomposition(lu, Permutation.identity(3))
    assertResult { Matrix(3,5)( 2,  4,  2,
                                3,  4,  2,
                                6,  6,  4,
                                8,  8, 20,
                               20, 18, 26) } { lud.reconstructedOriginal }
  }
}

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

  def decompositionGoodness(a:Matrix, lu:LU.Decomposition) = {
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
    if(normA <= 0) 1.0/eps
    else {
      // b := b - A X
      gemm(NoTranspose, NoTranspose, -1.0, a, x, 1.0, b)

      val (m, nRhs) = b.dimensions
      (for(j <- 0 until nRhs) yield {
        val normB = b.column(j).map(_.abs).sum
        val normX = x.column(j).map(_.abs).sum
        if(normX <= 0) 1.0/eps
        else ((normB/normA)/normX)/eps
        }).max
    }
  }

  //implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  implicit val gen = Defaults.IntegerGenerator.fromTime(1)

  test("LU decomposition and solving of system of linear equations") {
    //StringFormatting.useMathematicaFormat = true
    //StringFormatting.elementFormat = "%13.6g"
    val linTests = new LinearSystemTestMatrices(nonSpecialDimensions = 2)
    val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
    val elts = new RandomUncorrelatedElements(elements = uniformIm1p1)
    for((imat, zeroIdx, a) <- linTests.sample) {
      val a0 = a.copyToMatrix
      val (m,n) = a.dimensions

      //info(s"Type $imat, dimension $m x $n")
      try {
        val lu = TestedLUDecompositionConstruction(a)
        val goodnessLU = decompositionGoodness(a0, lu)
        assert(goodnessLU < threshold,
               s"""|Failure: $goodnessLU >= $threshold
                   |imat=$imat
                   |A=$a0
                   |$lu
                   |""".stripMargin)
        //info("\t+ Decomposition P A = L U")

        if(m == n && m > 1) {
          for(nRhs <- rightHandSides;
              x <- elts.generalMatrixSample(m, nRhs).take(1)) {
            // Construct rhs B = A X
            val b = Matrix.empty(m, nRhs)
            gemm(NoTranspose, NoTranspose, 1.0, a0, x, 0.0, b)

            // Solve A X = B and test residual
            val b0 = b.copyToMatrix
            lu.solve(NoTranspose, b)
            val residual = residualForSolution(a0, b0, b)
            assert(residual < threshold,
                   s"""|Failure $residual >= $threshold in solving A X = B
                       |imat=$imat
                       |LU=${lu.lu}
                       |B=$b0""".stripMargin)
          }
          //info("\t+ Solution of A X = B")
        }
      }
      catch {
        case ex:LU.Singularity =>
          assert(zeroIdx != None,
                 s"""|Unexpected singularity for matrix type #imat:
                     |$a0
                     |""".stripMargin)
          assertResult { zeroIdx.get } { ex.pivotIndex }
      }
    }
  }
}

trait LUDecompositionConstructionForTests extends LU.DecompositionConstruction
with BLAS.LayeredLevel3 with BLAS.NaiveLevel2 with BLAS.NaiveLevel1 {
  def raw(lu1:Matrix, p1:Permutation) =
    new LU.Decomposition
    with BLAS.LayeredLevel3 with BLAS.NaiveLevel2 with BLAS.NaiveLevel1 {
      val lu = lu1
      val p = p1
    }
}

class UnblockedLUDecomposition extends LUDecompositionTest {
  object TestedLUDecompositionConstruction
  extends LUDecompositionConstructionForTests
  with LU.UnblockedDecompositionConstruction
}

class RecursiveLUDecompositionTest extends LUDecompositionTest {
  object TestedLUDecompositionConstruction
  extends LUDecompositionConstructionForTests
  with LU.RecursiveDecompositionConstruction {
    // we chose this value because it fits at the right spot in the
    // range of tested sizes: 1 to 16. I.e. not trivial but small enough that
    // it would test the recursive mechanism.
    val unblockedThreshold = 4
  }
}

class ClassicLeftToRightBlockedLUDecompositionTest extends LUDecompositionTest {
  object TestedLUDecompositionConstruction
  extends LUDecompositionConstructionForTests
  with LU.ClassicLeftToRightBlockedDecompositionConstruction {
    // we chose this value because it fits at the right spot in the
    // range of tested sizes: 1 to 16. I.e. not trivial but small enough that
    // it would test the iterations over the panels.
    val nb = 4
  }
}

class FusedLeftToRightBlockedLUDecompositionTest extends LUDecompositionTest {
  object TestedLUDecompositionConstruction
  extends LUDecompositionConstructionForTests
  with LU.FusedLeftToRightBlockedDecompositionConstruction {
    // we chose this value because it fits at the right spot in the
    // range of tested sizes: 1 to 16. I.e. not trivial but small enough that
    // it would test the iterations over the panels.
    val nb = 4
    lazy val blocking = new GEBP.Blocking(mc=8, kc=4, mr=2, nr=4, np=256)
  }
}

