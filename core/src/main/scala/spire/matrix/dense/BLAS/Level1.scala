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
 *     in the arguments of type IndexedSeq.
 *
 */
package spire.matrix.BLAS.level1

import spire.matrix.dense.{VectorLike, PlaneRotation}

import spire.syntax.cfor._
import scala.collection.mutable

trait Interface {
  /**
   * Perform the vector operation
   *\[
   *     x := \alpha x
   *\]
   *
   */
  def scale(alpha: Double, x: mutable.IndexedSeq[Double]): Unit

  /** For a given vector x of length n, perform y(0:n) = x(0:n) */
  def copy(x: mutable.IndexedSeq[Double], y: mutable.IndexedSeq[Double]): Unit

  /**
   * Perform the vector operation
   * \[
   *     y := \alpha x + y
   * \]
   */
  def axpy(alpha: Double,
           x: mutable.IndexedSeq[Double], y: mutable.IndexedSeq[Double]): Unit

  /**
   * Transform each (x(i), y(i)) into its image by the given plane rotation
   *
   * This may be interpreted as one of the following matrix product, performed
   * in place:
   * <pre>
   *         [ x(0) y(0) ] G^T
   *         [ x(1) y(1) ]
   *         [ ......... ]
   * </pre>
   * or
   * <pre>
   *         G [ x(0) y(0) .... ]
   *           [ x(1) y(1) .... ]
   * </pre>
   * where G is this rotation and G^T its transpose (which is also its inverse).
   */
  def rot(x:VectorLike, y:VectorLike, g:PlaneRotation): Unit
}

trait Naive extends Interface {
  def scale(alpha: Double, x: mutable.IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => x(k) *= alpha }

  def copy(x: mutable.IndexedSeq[Double], y: mutable.IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => y(k) = x(k) }

  def axpy(alpha: Double,
           x: mutable.IndexedSeq[Double], y: mutable.IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => y(k) += alpha * x(k) }

  def rot(x:VectorLike, y:VectorLike, g:PlaneRotation) {
    cforRange(0 until x.length) { i =>
      val xi = g.cs*x(i) + g.sn*y(i)
      y(i) = g.cs*y(i) - g.sn*x(i)
      x(i) = xi
    }
  }
}
