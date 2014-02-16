package spire.matrix.dense.LU
import spire.matrix.dense._
import spire.matrix.{Sides, UpperOrLower, Transposition, DiagonalProperty}
import spire.matrix.NumericPropertiesOfDouble
import Sides._
import UpperOrLower._
import Transposition._
import DiagonalProperty._

import spire.syntax.cfor._

import scala.math.min

/**
 * LU decomposition of a general rectangular matrix
 *
 * Mathematically, the decomposition of a m x n matrix A reads
 *
 * A = P L U
 *
 * where P is a permutation matrix,  L is lower triangular with unit diagonal
 * elements (lower trapezoidal if m > n), and U is upper triangular (upper
 * trapezoidal if m < n). This decomposition is stored in the matrix lu as
 * follow:
 *  - L is stored below the diagonal of lu (the unit diagonal is not stored)
 *  - U is stored above the diagonal of lu, using the diagonal of lu
 *  - P is encoded in an array p following the convention that
 *    row i of the matrix was interchanged with row p(i); that array p is
 *    stored in member permutation.
 *
 * This class does not provide the mean to construct such a decomposition,
 * only to store a decomposition and perform operations on it (such as
 * solving a system of equations e.g.)
 *
 * References:
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
trait Decomposition extends BLAS.Level1 with BLAS.Level2 with BLAS.Level3
{
  val lu:Matrix

  val p:Permutation

  /**
   * Reconstruct the original matrix A by performing the product PLU.
   *
   * The decomposition is left untouched.
   *
   * Reference: subroutine xGET01 from [1]
   */
  def reconstructedOriginal: Matrix = {
    val (m, n) = lu.dimensions
    if(m == 1 || n == 1) return lu

    val plu = lu.copyToMatrix
    cforRange(n-1 to 0 by -1) { k =>
      if(k >= m) trmv(Lower, NoTranspose, UnitDiagonal,
                      plu.block(0,m)(0,m), plu.column(k))
      else {
        // compute elements (k+1:m, k)
        val t = plu(k,k)
        if(k+1 < m) {
          val col_k_head = plu.column(k).block(0  , k)
          val col_k_tail = plu.column(k).block(k+1, m)
          scale(t, col_k_tail)
          gemv(NoTranspose,
               1.0, plu.block(k+1,m)(0,k), col_k_head, 1.0, col_k_tail)
        }

        // compute element (k,k)
        plu(k,k) = t + dot(plu.row(k).block(0,k), plu.column(k).block(0,k))

        // compute elements (1:k-1, k)
        trmv(Lower, NoTranspose, UnitDiagonal,
             plu.block(0,k)(0,k), plu.column(k).block(0,k))
      }
    }
    p.inverse.permute_rows(plu.block(0, m)(0, n))
    plu
  }

  /**
   * Solve a system of linear equation
   *
   * The system of equations read in matrix form
   *
   *  1. A X = B  or
   *  2. A^T^ X = B
   *
   * The matrix A shall be square and B is overwritten by X.
   *
   * Reference: DGETRS from [1]
   */
  def solve(trans:Transposition.Value, b:Matrix) {
    require(lu.isSquare)
    if(trans == NoTranspose)
      require(lu.dimensions._1 == b.dimensions._1)
    else
      require(lu.dimensions._2 == b.dimensions._1)

    if(trans == NoTranspose) {
      p.permute_rows(b)
      trsm(FromLeft, Lower, NoTranspose, UnitDiagonal, 1.0, lu, b)
      trsm(FromLeft, Upper, NoTranspose, NonUnitDiagonal, 1.0, lu, b)
    }
    else {
      trsm(FromLeft, Upper, Transpose, NonUnitDiagonal, 1.0, lu, b)
      trsm(FromLeft, Lower, Transpose, NonUnitDiagonal, 1.0, lu, b)
      p.inverse.permute_rows(b)
    }
  }

  override def toString =
    s"""|L\\U=$lu
        |
        |P=$p
        |
        |""".stripMargin
}

/**
 * Exception thrown when attempting to decompose a singular matrix
 *
 * The decomposition is not valid in this case and it shall not be used
 * to solve equations.
 *
 * @pivotIndex shall be the smallest i such that U(i,i) == 0
 */
class Singularity(val pivotIndex:Int)
extends spire.matrix.dense.Exception

/** Construction of the LU decomposition of a rectangular matrix A */
trait DecompositionConstruction
extends BLAS.Level3 with BLAS.Level2 with BLAS.Level1 {

  /** Computes and returns the LU decomposition of A */
  def apply(a:Matrix) = {
    val (m,n) = a.dimensions
    val p = Permutation.identity(m)
    decompose(a, p)
    raw(a, p)
  }

  /** Shall perform the actual decomposition */
  protected def decompose(a:Matrix, p:Permutation)

  /**
   * Factory method to act as a polymorphic constructor for Decomposition heirs
   */
  protected def raw(lu:Matrix, p:Permutation): Decomposition
}


/**
 * LU decomposition with straightforward unblocked algorithm
 *
 * Reference: DGETF2 from LAPACK
 */
trait UnblockedDecompositionConstruction
extends DecompositionConstruction with NumericPropertiesOfDouble {

  protected def decompose(a:Matrix, p:Permutation) {
    val (m,n) = a.dimensions
    val mn = min(m,n)
    cforRange(0 until mn) { j =>
      val jp = j + idamax(a.column(j).block(j,m))
      p(j) = jp
      if(a(jp, j) == 0) throw new Singularity(j)
      cforRange(0 until n) { k =>
        val t = a(jp,k); a(jp,k) = a(j,k); a(j,k) = t
      }
      val pivot = a(j,j)
      if(j < m-1) {
        if (pivot.abs >= safeMinimum)
          scale(1/pivot, a.column(j).block(j+1,m))
        else
          cforRange(j+1 until m) { i => a(i,j) /= pivot }
      }
      if(j < mn - 1)
        ger(-1.0, a.column(j).block(j+1,m), a.row(j).block(j+1,n),
            a.block(j+1,m)(j+1,n))
    }
  }
}
/**
 * LU decomposition implemented with Sivan Toledo's recursive algorithm
 *
 * The mathematical theory and especially the comparison with blocked LU
 * decomposition can be found in [1] whereas the code of an explicitely
 * recursive version can be found in [2, example 14-4]. LAPACK features
 * an iterative version of this algorithm in its VARIANTS/REC sub-directory.
 *
 *
 * References:
 *
 * [1] Sivan Toledo,
 *     Locality of reference in LU decomposition with partial pivoting,
 *     SIAM Journal on Matrix Analysis
 *     and Applications 18 (1997), no. 4, 1065–1081.
 *
 * [2] Andy Oram and Greg Wilson (eds.),
 *     Beautiful code: Leading programmers explain how they think,
 *     O’Reilly, 2007.
 */
trait RecursiveDecompositionConstruction
extends DecompositionConstruction with UnblockedDecompositionConstruction {

  val unblockedThreshold: Int

  /** The recursive function that actually performs the decomposition */
  protected override def decompose(a:Matrix, p:Permutation) {
    val (m,n) = a.dimensions
    val mn = min(m,n)
    if(mn > unblockedThreshold) {
      /* A = [ A11 A12 ]
             [ A21 A22 ]
        where A11 is n/2 x n/2
      */
      val nLeft = mn/2

      /* Recursively factor P1 [ A11 ] = [ L11 ] U11
                               [ A21 ]   [ L21 ]
      */
      decompose(a.block(0, m)(0, nLeft), p)
      val l11 = a.block(0, nLeft)(0, nLeft) // was A11
      val l21 = a.block(nLeft, m)(0, nLeft) // was A21

      /* Permute [ A12' ] = P1 [ A12 ]
                 [ A22' ]      [ A22 ]
         Suffix p means prime in the following
      */
      p.permute_rows(a.block(0, m)(nLeft, n))
      val a12p = a.block(0, nLeft)(nLeft, n) // was A12
      val a22p = a.block(nLeft, m)(nLeft, n) // was A22

      /* Solve for U12: L11 U12 = A12' in place */
      trsm(FromLeft, Lower, NoTranspose, UnitDiagonal, 1.0, l11, a12p)
      val u12 = a12p
      /* A22'' = A22' - L21 U12 in place */
      gemm(NoTranspose, NoTranspose, -1.0, l21, u12, 1.0, a22p)
      val a22pp = a22p

      /* Recursively factor P2 A22'' = L22 U22
         We need to catch failure on the way out so as to update the
         index the zero pivot.
      */
      val p2 = p.subset(nLeft,m)
      try decompose(a22pp, p2)
      catch {
        case ex: Singularity => throw new Singularity(ex.pivotIndex + nLeft)
      }

      /* Permute L21' = P2 L21 */
      p2.permute_rows(l21)
    }
    else super[UnblockedDecompositionConstruction].decompose(a, p)
  }
}
