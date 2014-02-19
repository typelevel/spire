package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Transposition._
import UpperOrLower._
import Sides._
import DiagonalProperty._
import spire.matrix.NumericPropertiesOfDouble
import spire.matrix.dense.BLAS
import spire.matrix.dense.random._
import spire.implicits._
import scala.math


/** General rectrangular matrix */
class GeneralMatrix(m:Int, n:Int, a:Array[Double])
extends Matrix(m, n, m, 0, a)
with NumericPropertiesOfDouble with MagnitudeLimitation {}

object GeneralMatrix extends NumericPropertiesOfDouble {
  def zero(m:Int, n:Int) = new GeneralMatrix(m, n, new Array[Double](m*n))
}


/**
 * Random matrices to test linear system solving
 *
 * References:
 *
 * [1] James W. Demmel and Alan McKenney,
 *     A test matrix generation suite,
 *     Tech. Report 9, LAPACK Working Note,
 *     March 1989.
 *
 * [2] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
class LinearSystemTestMatrices(
  nonSpecialDimensions:Int = 0,
  randomMatricesOfEachType:Int = 1,
  excludedTypes:Set[Int] = Set(),
  includedTypes:Set[Int] = (1 to 11).toSet)
  (implicit gen:Defaults.IntegerGenerator)
extends TestDimensions(nonSpecialDimensions) with NumericPropertiesOfDouble
{
  val types = includedTypes -- excludedTypes -- Set(2,3)

  val eps = precision
  val veryBadConditionNumber = 0.1/eps
  val badConditionNumber = math.sqrt(veryBadConditionNumber)
  val small = safeMinimum
  val large = 1/small

  type SampleType = Traversable[(Int, Option[Int], Matrix)]

  /**
   * A sample of m x n matrices
   *
   * This is actually a sequence of triplet (imat, zeroIndex, mat) where mat
   * is the m x n matrix to test whereas imat is an integer code specifying the
   * properties of mat, as follow:
   *
   * - shape:
   *   + imat=1: diagonal
   *   + imat=2: upper triangular (not implemented)
   *   + imat=3: lower triangular (not implemented)
   *   + otherwise: general matrix
   *
   * - condition number:
   *   + imat=8: very bad condition number
   *   + imat=9: bad condition number
   *   + otherwise: the condition number is 2
   *
   * - matrix norm:
   *   + imag=10: small
   *   + imag=11: large
   *   + otherwise: 1
   *
   * - zeroed columns:
   *   + imat=5: first one
   *   + imat=6: last one
   *   + imat=7: all columns right of, and including, min(m,n)/2
   *
   * ZeroIndex is the index of the first zero column or None if the matrix
   * is not singular.
   *
   * Reference: [1] and subroutine DCHKGE from [2]
   */
  def sample(m:Int, n:Int)(
    implicit work:Scratchpad = new Scratchpad(
      OrthogonalMatricesHaarDistribution.minimumScratchpad(math.max(m,n))))
  : SampleType = new SampleType {

    val orthogonalLeft = new OrthogonalMatricesHaarDistribution(m)
    val orthogonalRight = new OrthogonalMatricesHaarDistribution(n)

    // if n is too small, the test cases with a zeroed column
    // are atypical, so skip them
    def shallSkip(imat:Int, m:Int, n:Int) =
      5 <= imat && imat <= 7 && n < imat - 4

    def foreach[U](f: ((Int, Option[Int], Matrix)) => U) {
      for(imat <- 1 to 11 if types.contains(imat) && !shallSkip(imat, m, n)) {
        val conditionNumber = imat match {
          case 8 => veryBadConditionNumber
          case 9 => badConditionNumber
          case _ => 2.0
        }
        val norm = imat match {
          case 10 => small
          case 11 => large
          case _  => 1.0
        }
        // Set diagonal
        val diag = new SpecialDiagonal(math.min(m, n),
                                       SpecialDiagonalMode.Geometric,
                                       conditionNumber)
        // Rescale to get the desired norm of the final matrix
        diag.rescaleElementMagnitudeTo(norm)
        val a = GeneralMatrix.zero(m,n)
        a.diagonal := diag

        if(imat != 1) {
          // A := U A V where U and V are random orthogonal matrices
          orthogonalLeft.overwriteWithProductByNext(FromLeft, a)
          orthogonalRight.overwriteWithProductByNext(FromRight, a)
        }

        // Perhaps zero some columns
        val zeroIdx = imat match {
          case 5 => {
            val i = 0
            a.column(i) := 0
            Some(i)
          }
          case 6 => {
            val i = math.min(m, n) - 1
            a.column(i) := 0
            Some(i)
          }
          case 7 => {
            val i = math.min(m, n)/2
            a.block(0,m)(i,n) := 0
            Some(i)
          }
          case _ => None
        }

        // yield
        f((imat, zeroIdx, a))
      }
    }
  }

  /** A sample of matrix of various sizes */
  def sample:SampleType = new SampleType {
    def foreach[U](f: ((Int, Option[Int], Matrix)) => U) {
      for((m,n) <- twoDimensionSample) {
        for((imat, zeroIdx, a) <- sample(m,n)) f((imat, zeroIdx, a))
      }
    }
  }
}
