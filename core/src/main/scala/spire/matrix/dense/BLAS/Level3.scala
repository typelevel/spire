/**
 * An implementation of the infamous Basic Linear Algebra System, level 3
 *
 * BLAS level 3 is concerned with those basic operations
 * that references O(n^2) elements and that performs O(n^3) flops,
 * where n is the size of the problem. Those operations are:
 *
 *   - multiplication of a pair of matrices, supporting different type for
 *     each of them (general, symmetric, triangular, etc)
 *   - solution of triangularised system of equations for many right-hand sides
 *
 * The BLAS function declarations are altered as follow:
 *
 *   - we remove only the leading character (S, D, Z) indicating the type
 *     of elements, therefore keeping the next 2 characters indicating the type
 *     of matrices (GE for general, SY for symmetric, TR for triangular, ...)
 *   - we do not pass the matrix sizes and leading dimension as arguments
 *     as they are encapsulated in the matrix arguments
 *   - the dummy CHARACTER*1 parameters such as TRANx are coded with
 *     enumerations instead
 *
 */
package spire.matrix.BLAS.level3

import spire.matrix.dense.MatrixLike
import spire.matrix.BLAS._
import Transposition._

trait interface {
  /**
   * Performs the matrix-matrix operations
   *
   * \[
   *     C := \alpha op(A) op(B) + \beta C
   * \]
   *
   * where $op(X) = X$ if transX is NoTranspose
   * or $op(X) = X^T$ if transX is Transpose or ConjugateTranspose.
   *
   * The matrix C is thus modified in-place whereas matrices a and b are
   * not modified. Result is unpredictable if C overlaps with any of A or B.
   *
   * TODO: we could check whether there are such overlaps
   */
  def GEMM(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:MatrixLike, b:MatrixLike,
           beta:Double, c:MatrixLike): Unit
}


/** Straightforward but inefficient implementations
  *
  * No blocking, no parallelism, no vectorisation(?).
  */
trait naive extends interface {

  def GEMM(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:MatrixLike, b:MatrixLike,
           beta:Double, c:MatrixLike): Unit = {
    require(if(transA == NoTranspose) c.dimensions._1 == a.dimensions._1
            else                      c.dimensions._1 == a.dimensions._2)
    require(if(transB == NoTranspose) c.dimensions._2 == b.dimensions._2
            else                      c.dimensions._2 == b.dimensions._1)
    require(if(transA == NoTranspose && transB == NoTranspose) {
              a.dimensions._2 == b.dimensions._1
            }
            else if(transA == NoTranspose && transB != NoTranspose) {
              a.dimensions._2 == b.dimensions._2
            }
            else if(transA != NoTranspose && transB == NoTranspose) {
              a.dimensions._1 == b.dimensions._1
            }
            else if(transA != NoTranspose && transB != NoTranspose) {
              a.dimensions._1 == b.dimensions._2
            }
            else false)

    val (m, n) = c.dimensions
    val k = if(transB == NoTranspose) b.dimensions._1 else b.dimensions._2

    // trivial cases
    if(alpha == 0) {
      if(beta == 0)
        for(j <- 0 until n; i <-0 until m) c(i,j) = 0
      else if(beta != 1)
        for(j <- 0 until n; i <- 0 until m) c(i,j) *= beta
      return
    }

    // charge!
    if (transB == NoTranspose) {
      if(transA == NoTranspose) {
        // (I) C := alpha A B + beta C
        for(j <- 0 until n) {
          if(beta == 0)
            for(i <- 0 until m) c(i,j) = 0
          else if(beta != 1)
            for(i <- 0 until m) c(i,j) *= beta
          for(l <- 0 until k) {
            if(b(l,j) != 0) {
              val t = alpha*b(l,j)
              for(i <- 0 until m) c(i,j) += t*a(i,l)
            }
          }
        }
      }
      else {
        // (II) C := alpha A^T B + beta C
        for(j <- 0 until n; i <- 0 until m) {
          var t = 0.0
          for(l <- 0 until k) t += a(l,i)*b(l,j)
          if(beta == 0) c(i,j) = alpha*t
          else          c(i,j) = alpha*t + beta*c(i,j)
        }
      }
    }
    else {
      if(transA == NoTranspose) {
        // (III) alpha A B^T + beta C,
        // same as (I) except that b(l,j) becomes b(j,l)
        for(j <- 0 until n) {
          if(beta == 0)
            for(i <- 0 until m) c(i,j) = 0
          else if(beta != 1)
            for(i <- 0 until m) c(i,j) *= beta
          for(l <- 0 until k) {
            if(b(j,l) != 0) {
              val t = alpha*b(j,l)
              for(i <- 0 until m) c(i,j) += t*a(i,l)
            }
          }
        }
      }
      else {
        // (IV) alpha A^T B^T + beta C,
        // same as (II) except that b(l,j) becomes b(j,l)
        for(j <- 0 until n; i <- 0 until m) {
          var t = 0.0
          for(l <- 0 until k) t += a(l,i)*b(j,l)
          if(beta == 0) c(i,j) = alpha*t
          else          c(i,j) = alpha*t + beta*c(i,j)
        }
      }
    }

  }

}

