package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.NumericPropertiesOfDouble
import spire.matrix.Transposition._
import spire.matrix.UpperOrLower._
import spire.matrix.dense.BLAS
import spire.matrix.dense.random._

import scala.math._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

trait NonSymmetricEigenProblemTestLike extends FunSuite
with CommonMatrixPropertyTests
with BLAS.NaiveLevel3
{
  val HessenbergDecomposition: Hessenberg.DecompositionLikeCompanion
  val SchurDecomposition: Schur.RealDecompositionCompanion

  def decompositionGoodness(a:MatrixLike, q:MatrixLike, h:MatrixLike) = {
    val (m,n) = q.dimensions
    val qh = Matrix.empty(m, n)
    gemm(NoTranspose, NoTranspose, 1.0, q, h, 0.0, qh)
    val d = a.copyToMatrix
    gemm(NoTranspose, Transpose, -1.0, qh, q, 1.0, d)
    // Reference: subroutine DHST01 in LAPACK
    val underflow = safeMinimum
    val smallNum = underflow*n/eps
    val aNorm = max(a.norm1, underflow)
    min(d.norm1, aNorm) / max(smallNum, aNorm*eps) / n
  }

  test("Hessenberg decomposition of decoupled diagonal blocks") {
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(3)
      + HessenbergDecomposition.unblockedMinimumScratchpad(8))

    // This is not a precision test, just a corner case test
    info("Simple 3x3 block")
    val m0 = Matrix(3,3)(100, 500, 700,
                         300,   0, 600,
                         400, 300, 100)
    val h = HessenbergDecomposition.withUnblockedAlgorithm(m0.copyToMatrix)()
    expectResult(Matrix(3,3)( 100, -860,   20,
                             -500,  496,  -72,
                                0,  228, -396))(h.reducedMatrix)
    expectResult(Matrix(3,3)(1.0,  0.0 ,   0.0 ,
                             0.0, -0.60,  -0.80,
                             0.0, -0.80,   0.60)) {
      h.transformationWithUnblockedAlgorithm.round(9)
    }

    info("Previous 3x3 block embedded in 8x8 matrix")
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
    val h1 = HessenbergDecomposition.withUnblockedAlgorithm(m1)()
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

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)

  test("All steps of unblocked eigen-decomposition") {
    val eigenTests = new EigenTestMatrices
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(
        eigenTests.oneDimensionSample.max))
    for((itype, a) <- eigenTests.sample) {
      val n = a.dimensions._1
      val a0 = a.copyToMatrix

      info(s"Type $itype, dimension $n")
      info("\tHessenberg")
      val hd = HessenbergDecomposition.withUnblockedAlgorithm(a)()
      val q = hd.transformationWithUnblockedAlgorithm
      val h = hd.reducedMatrix
      // That value 20 is what LAPACK nep.in sets by default
      assert(orthogonalityMeasure(q) < 20)
      val goodnessHessenberg = decompositionGoodness(a0, q, h)
      assert(goodnessHessenberg < 20,
             s"""|
                 |type $itype
                 |goodness=$goodnessHessenberg
                 |A=$a0
                 |Q=$q
                 |H=$h
                 |""".stripMargin)

      info("\tSchur")
      val q1 = Matrix.identity(n)
      val sd = try {
        SchurDecomposition(a, Some(q1), fullSchurFormWanted=true)()()
      }
      catch {
        case e: Schur.Decomposition.ConvergenceFailure => {
          info(s"H=$h")
          throw e
        }
      }
      val z = sd.z.get
      val t = sd.t.get
      assert(orthogonalityMeasure(z) < 20)
      val goodnessSchur = decompositionGoodness(h, z, t)
      assert(goodnessSchur < 20,
             s"""|
                 |type $itype
                 |goodness=$goodnessSchur
                 |H=$h
                 |Z=$z
                 |T=$t
                 |""".stripMargin)
    }
  }
}

class NonSymmetricEigenProblemWithReferenceAlgorithmsAndNaiveBLASTest
extends NonSymmetricEigenProblemTestLike
{
  val HessenbergDecomposition = Hessenberg.DecompositionWithNaiveBLAS
  val SchurDecomposition = Schur.RealDecompositionWithDoubleShiftQRAlgorithm
}

class SchurDecomposition2x2Test
extends FunSuite with ShouldMatchers with NumericPropertiesOfDouble
{
  val eps = 2*precision

  test("Upper Triangular") {
    val d = Schur.Decomposition2x2(1.0, -1.0,
                                   0.0,  2.0)
    d.r  should be === PlaneRotation.identity
    d.schurForm  should be === (1.0, -1.0,
                                0.0,  2.0)
    d.eigenvalues  should be === ((1.0, 0), (2.0, 0))
  }

  test("Lower Triangular") {
    val d = Schur.Decomposition2x2(-1.0, 0.0,
                                    4.0, 1.0)
    d.r  should be === PlaneRotation.positiveQuarterRotation
    d.schurForm  should be === (1.0, -4.0,
                                0.0, -1.0)
    d.eigenvalues  should be === ((1.0, 0), (-1.0, 0))
  }

  test("Schur Form Already") {
    val d = Schur.Decomposition2x2(1.0, -3.0,
                                   3.0,  1.0)
    d.r  should be === PlaneRotation.identity
    d.schurForm  should be === (1.0, -3.0,
                                3.0,  1.0)
    val ((re1, im1), (re2, im2)) = d.eigenvalues
    re1 should be ( 1.0 plusOrMinus eps)
    im1 should be ( 3.0 plusOrMinus eps)
    re2 should be ( 1.0 plusOrMinus eps)
    im2 should be (-3.0 plusOrMinus eps)
  }

  test("Complex conjugate eigenvalues") {
    val sd = Schur.Decomposition2x2(1 + 3*sqrt(3)/4, 7.0/4,
                                            -13.0/4, 1 - 3*sqrt(3)/4)

    toDegrees(sd.r.angle) should be (30.0 plusOrMinus toDegrees(eps))

    val (a, b,
         c, d) = sd.schurForm
    a should be ( 1.0 plusOrMinus eps)
    b should be ( 1.0 plusOrMinus eps)
    c should be (-4.0 plusOrMinus eps)
    d should be ( 1.0 plusOrMinus eps)

    val ((re1, im1), (re2, im2)) = sd.eigenvalues
    re1 should be ( 1.0 plusOrMinus eps)
    im1 should be ( 2.0 plusOrMinus eps)
    re2 should be ( 1.0 plusOrMinus eps)
    im2 should be (-2.0 plusOrMinus eps)
  }

  test("Clearly distinct real eigenvalues") {
    val s3 = sqrt(3)
    val sd = Schur.Decomposition2x2(( 3 - s3)/2, (1 + s3)/2,
                                    (-3 + s3)/2, (5 + s3)/2)
    toDegrees(sd.r.angle) should be (-165.0 plusOrMinus toDegrees(eps))
    val (a, b,
         c, d) = sd.schurForm
    a should be (1.0 plusOrMinus eps)
    b should be (2.0 plusOrMinus eps)
    c should be (0.0 plusOrMinus eps)
    d should be (3.0 plusOrMinus eps)
  }

  test("Degenerate real eigenvalues") {
    // This is slippery business as the rounding error during the construction
    // of the matrix may shift from this case to the complex
    // conjugate case. This example therefore chooses carefully the coefficient
    // to avoid round-offs in the last representative digit.
    val s3 = sqrt(3)
    val sd = Schur.Decomposition2x2(1.0 - s3/8,      1.0/8,
                                        -3.0/8, 1.0 + s3/8)
    toDegrees(sd.r.angle) should be (60.0 plusOrMinus toDegrees(eps))
    val (a, b,
         c, d) = sd.schurForm
    a should be (1.0 plusOrMinus eps)
    b should be (0.5 plusOrMinus eps)
    c should be (0.0 plusOrMinus eps)
    d should be (1.0 plusOrMinus eps)
  }

  test("Nearly-degenerate real eigenvalues") {
    // This tests the very last "if" branch of Schur.Decomposition2x2.apply
    val sd = Schur.Decomposition2x2(1     , 0.5,
                                    1e-16 , 1   )
    toDegrees(sd.r.angle) should be (
      8.1028468454139541e-7 plusOrMinus toDegrees(eps))
    val (a, b,
         c, d) = sd.schurForm
    a should be (1.00000000707106781 plusOrMinus eps)
    b should be (0.499999999999999900 plusOrMinus eps)
    c should be === 0.0
    d should be (0.999999992928932188 plusOrMinus eps)
    // That would be true if the branch mentionned above has been taken
    a - 1 should be (1 - d plusOrMinus eps)
  }
}
