package spire.matrix.dense.Hessenberg

import spire.matrix.dense._
import scala.math._

/**
 * Hessenberg decomposition.
 *
 * A Hessenberg decomposition reads
 * \[
 *     A = Q H Q^T
 * \]
 * where Q is an orthogonal matrix and H is a Hessenberg matrix.
 * Q is represented as a product of n elementary reflectors
 * \[
 *     Q = H_{n-1} H_{n-2} \cdots H_0
 * \]
 * Each elementary reflector has the form $H_i = I - \tau_i v_i v_i^T$
 * (@see trait ElementaryReflectorLike).
 *
 * This class handles the storage of and the operation on the decomposion
 * $Q H Q^T$ but not the computation of Q and H given an input matrix A
 * (this is the role of the trait HessenbergDecompositionConstruction).
 * The decomposition is stored in the matrix A, and in an extra vector tau
 * of length n:
 *
 *  - it is assumed that the blocks A(0:iLo, 0:iLo) and A(iHi:n, iHi:n) are
      upper triangular whereas A(iLo:n, 0:iLo) and A(iHi:n, 0:iHi) are zero.
 *  - the first subdiagonal and the part of A(iLo:iHi, iLo:iHi) above it
 *    contains the elements of the Hessenberg matrix H
 *  - each column j below the first subdiagonal of A(iLo:iHi, iLo:iHi)
 *    contains the j-2 elements of the essential part of an elementary
 *    reflector $h_j$ such that
 *    \[
 *        Q(iLo:iHi, iLo:iHi) = h_{iHi-2} h_{iHi-3} \cdots h_{iLo}.
 *    \]
 *  - the $tau_i$ are stored in `tau`.
 * *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
trait DecompositionLike {

  protected val a:Matrix
  require(a.isSquare)

  protected val iLo:Int
  protected val iHi:Int

  protected val taus:Vector

  protected val ElementaryReflector:ElementaryReflectorLikeCompanion

  /**
   * The matrix H
   *
   * The elements are copied from the decomposition stored in A
   * into a new matrix.
   */
  def reducedMatrix = a.copyToUpperHessenberg

  /**
   * The matrix $Q$ formed by computing the product of the $H_i$.
   *
   * This uses an unblocked algorithm and it does not work in-place.
   * Thus this should only be used as a reference implementation for
   * debugging purposes.
   *
   * Reference: subroutine DORG2R in LAPACK [1] (this routines is for
   * QR decomposition but we trivially adapted it to the layout of the
   * elementary reflectors for the Hessenberg decomposition)
   */
  def transformationWithUnblockedAlgorithm(implicit work:Scratchpad):Matrix =
  {
    val n = a.dimensions._1
    val result = Matrix.identity(n)

    // Accumulate reflectors backward
    for(j <- iHi-3 to iLo by -1) {
      val h = ElementaryReflector(taus(j-iLo), a.column(j).block(j+2, iHi))
      h.applyOnLeft(result.block(j+1, iHi)(j+1, n))
    }
    result
  }
}

/**
 * Construction of the Hessenberg decomposition of a matrix in-place.
 *
 * Upon completion of each of the featured algorithms, A is overwritten by
 * the Hessenberg matrix H and the essential part of each elementary reflector
 * as specified by trait HessenbergDecompositionLike.
 *
 * Reference: subroutine DGEHD2 in LAPACK [1]
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 *
 */
trait DecompositionLikeCompanion {

  val ElementaryReflector: ElementaryReflectorLikeCompanion

  def apply(a:Matrix, iLo:Int, iHi:Int, tau:Vector): DecompositionLike

  /**
   * Decompose the n x n matrix A using an unblocked algorithm.
   *
   * This unblocked algorithm is suboptimal on modern computer
   * and it should therefore never be used by client code. It is however used
   * by the blocked algorithm to handle the corner cases.
   *
   * Reference: subroutine DGEHD2 in LAPACK [1]
   */
  def withUnblockedAlgorithm(a:Matrix)(iLo:Int = 0, iHi:Int = a.dimensions._1)
                            (implicit work: Scratchpad): DecompositionLike = {
    require(0 <= iLo && iLo < a.dimensions._1)
    require(iLo <= iHi && iHi <= a.dimensions._1)

    val n = a.dimensions._1
    val taus = Vector.empty(max(iHi-2 - iLo, 0))
    for(j <- iLo until iHi - 2) {
      val col = a.column(j).block(j+1, iHi)
      val h = ElementaryReflector.annihilateAndConstruct(col)
      taus(j-iLo) = h.tau
      h.applyOnRight(a.block(0, iHi)(j+1, iHi))
      h.applyOnLeft(a.block(j+1, iHi)(j+1, n))
    }
    this(a, iLo, iHi, taus)
  }

  /**
   * The minimum amound of working space needed by the unblocked algorithms
   */
  def unblockedMinimumScratchpad(n:Int) = ScratchpadSpecs(vectorLength=n)
}

class DecompositionWithNaiveBLAS(val a:Matrix,
                                           val iLo:Int, val iHi:Int,
                                           val taus:Vector)
extends DecompositionLike {
  val ElementaryReflector = ElementaryReflectorWithNaiveBLAS
}

object DecompositionWithNaiveBLAS
extends DecompositionLikeCompanion {
  val ElementaryReflector = ElementaryReflectorWithNaiveBLAS
  def apply(a:Matrix, iLo:Int, iHi:Int,
            taus:Vector): DecompositionLike =
    new DecompositionWithNaiveBLAS(a, iLo, iHi, taus)
}
