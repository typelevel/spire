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
import scala.math.{sqrt, min, max}


/** General rectrangular matrix */
class GeneralMatrix(m:Int, n:Int)(a:Array[Double])
extends Matrix(m, n)(a)
with NumericPropertiesOfDouble with MagnitudeLimitation {}

object GeneralMatrix extends NumericPropertiesOfDouble {
  def zero(m:Int, n:Int) = new GeneralMatrix(m,n)(new Array[Double](m*n))
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
  val types = includedTypes -- excludedTypes

  val eps = precision
  val badConditionNumber = {
    val c2 = 0.1/eps
    (sqrt(c2), c2)
  }
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
   * 1. Diagonal matrix
   * 2. Upper diagonal matrix
   * 3. Lower diagonal matrix
   * 4. General matrix, well-conditioned, with a norm of the order of 1
   * 5. Same as 4 but the first column is zero
   * 6. Same as 4 but the last column is zero
   * 7. Same as 4 but all columns right of, and including, min(m, n)/2 are zero
   * 8. Mildly ill-conditioned general matrix
   * 9. More ill-conditioned general matrix
   * 10. General matrix with small elements overhaul
   * 11. General matrix with large elements overhaul
   *
   * ZeroIndex is the index of the first zero column or None if the matrix
   * is not singular.
   *
   * Reference: [1] and subroutine DCHKGE from [2]
   */
  def sample(m:Int, n:Int)(
    implicit work:Scratchpad = new Scratchpad(
      OrthogonalMatricesHaarDistribution.minimumScratchpad(max(m,n))))
  : SampleType = new SampleType {

    val orthogonalLeft = new OrthogonalMatricesHaarDistribution(m)
    val orthogonalRight = new OrthogonalMatricesHaarDistribution(n)

    // if n is too small, the test cases with a zeroed column
    // are atypical, so skip them
    def shallSkip(imat:Int, m:Int, n:Int) =
      5 <= imat && imat <= 7 && n < imat - 4

    def foreach[U](f: ((Int, Option[Int], Matrix)) => U) {
      // TODO: code types 1 (diagonal), 2 (lower diagonal), and 3 (upper diagonal)
      for(imat <- 4 to 11 if types.contains(imat) && !shallSkip(imat, m, n)) {
        val conditionNumber = imat match {
          case 8 => badConditionNumber._1
          case 9 => badConditionNumber._2
          case _ => 2.0
        }
        val norm = imat match {
          case 10 => small
          case 11 => large
          case _  => 1.0
        }
        // Set diagonal
        val a = GeneralMatrix.zero(m,n)
        a.diagonal := new SpecialDiagonal(math.min(m, n),
                                          SpecialDiagonalMode.Geometric,
                                          conditionNumber)

        // Rescale to get the desired norm
        a.rescaleElementMagnitudeTo(norm)

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
