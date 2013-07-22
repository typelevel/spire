/**
 * An implementation of the infamous Basic Linear Algebra System, level 2
 *
 * BLAS level 2 is concerned with those basic operations
 * that references O(n^2) elements and that performs O(n^2) flops,
 * where n is the size of the problem. Those operations are:
 *
 *   - product of a matrix and a vector
 *   - rank-1 updates
 *
 * The BLAS function declarations are altered as follow:
 *
 *   - we remove only the leading character (S, D, Z) indicating the type
 *     of elements, therefore keeping the next 2 characters indicating the type
 *     of matrices (GE for general, SY for symmetric, TR for triangular, ...)
 *   - we do not pass either the matrix and vector sizes, or vector stride,
 *     or matrix leading dimension as arguments as they are encapsulated
 *     in the matrix and vector arguments
 *   - the dummy CHARACTER*1 parameters such as TRANx are coded with
 *     enumerations instead
 *
 */
package spire.linearAlgebra.BLAS.level2

import spire.linearAlgebra.dense.MatrixLike
import spire.linearAlgebra.dense.VectorLike
import spire.linearAlgebra.BLAS._
import Transposition._

trait interface {

  /**
   * This performs the following operation involving the matrix A,
   * the vectors x and y and the scalars $\alpha$ and $\beta$
   * \[
   *     y := \alpha op(A) x + beta y
   * \]
   * where $op(A)=A$ or $op(A)=A^T$ depending on the value
   * of the argument `trans`.
   */
   def GEMV(trans:Transposition.Value,
            alpha:Double, a:MatrixLike,
            x:VectorLike, beta:Double, y:VectorLike): Unit

   /**
    * This perform the rank-1 update
    * \[
    *     A := alpha x y^T + A
    * \]
    * where A is a matrix, x and y are vectors and $\alpha$ is a scalar.
    */
   def GER(alpha:Double, x:VectorLike, y:VectorLike, a:MatrixLike):Unit
}

trait naive {

   def GEMV(trans:Transposition.Value,
            alpha:Double, a:MatrixLike,
            x:VectorLike, beta:Double, y:VectorLike): Unit = {
    require(if(trans == NoTranspose) y.length == a.dimensions._1 &&
                                     a.dimensions._2 == x.length
            else                     y.length == a.dimensions._2 &&
                                     a.dimensions._1 == x.length)

    // y := beta y
    if(beta == 0) {
      for(i <- 0 until y.length) y(i) = 0
    }
    else if(beta != 1) {
      for(i <- 0 until y.length) y(i) *= beta
    }

    // y += alpha op(A) x
    if(alpha != 0) {
      val (m,n) = a.dimensions
      if(trans == NoTranspose) {
        for(j <- 0 until n) {
          if(x(j) != 0) {
            val t = alpha*x(j)
            for(i <- 0 until m) y(i) += t*a(i,j)
          }
        }
      }
      else {
        for(j <- 0 until n) {
          val t = (for(i <- 0 until m) yield a(i,j)*x(i)).sum
          y(j) += alpha*t
        }
      }
    }
  }

  def GER(alpha:Double, x:VectorLike, y:VectorLike, a:MatrixLike):Unit = {
    require((x.length, y.length) == a.dimensions)

    for(j <- 0 until y.length) {
      if(y(j) != 0) {
        val t = alpha*y(j)
        for(i <- 0 until x.length) a(i,j) += x(i)*t
      }
    }
  }
}
