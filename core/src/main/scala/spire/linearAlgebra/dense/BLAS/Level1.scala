/**
 * An implementation of the infamous Basic Linear Algebra System, level 1
 *
 * BLAS level 1 is concerned with those basic operations
 * that references O(n) elements and that performs O(n) flops,
 * where n is the size of the problem. Those operations are:
 *
 *   - vector scaling
 *   - linear combination of two vectors
 *   - vector dot product and norm
 *   - plane rotations
 *
 * The BLAS function declarations are altered as follow:
 *
 *   - we remove only the leading character (S, D, Z) indicating the type
 *     of elements, therefore keeping the next 2 characters indicating the type
 *     of matrices (GE for general, SY for symmetric, TR for triangular, ...)
 *   - we do not pass either the vector size or stride as they are encapsulated
 *     in the arguments of type VectorLike.
 *
 */
package spire.linearAlgebra.BLAS.level1

import spire.linearAlgebra.dense.VectorLike

trait interface {
  /**
   * Perform the vector operation
   *\[
   *     x := \alpha x
   *\]
   *
   */
  def SCALE(alpha:Double, x:VectorLike): Unit

  /** For a given vector x of length n, perform y(0:n) = x(0:n) */
  def COPY(x:VectorLike, y:VectorLike): Unit

  /**
   * Perform the vector operation
   * \[
   *     y := \alpha x + y
   * \]
   */
  def AXPY(alpha:Double, x:VectorLike, y:VectorLike): Unit
}

trait naive extends interface {
  def SCALE(alpha:Double, x:VectorLike): Unit = {
    for(k <- 0 until x.length) x(k) *= alpha
  }

  def COPY(x:VectorLike, y:VectorLike): Unit = {
    for(k <- 0 until x.length) y(k) = x(k)
  }

  def AXPY(alpha:Double, x:VectorLike, y:VectorLike): Unit = {
    for(k <- 0 until x.length) y(k) += alpha*x(k)
  }
}