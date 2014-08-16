package spire.matrix.dense

/**
 * The infamous BLAS (Basic Linear Algebra System)
 *
 * This provides the basic vector-vector, matrix-vector and matrix-matrix
 * operations that linear algebra algorithms require for their implementation.
 * They are classified in level 1, level 2 and level 3.
 *
 * The FORTRAN BLAS function declarations are altered as follow:
 *   - we remove only the leading character (S, D, Z) indicating the type
 *     of elements, therefore keeping the next 2 characters indicating the type
 *     of matrices (GE for general, SY for symmetric, TR for triangular, ...);
 *   - we pass [[scala.collection.mutable.IndexedSeq]],
 *     [[spire.matrix.dense.Vector]] and [[spire.matrix.dense.Matrix]]
 *     objects as arguments instead of raw pointers along with sizes, strides
 *     or leading dimensions as those are encapsulated in the arguments in our
 *     design;
 *   - dummy CHARACTER*1 parameters are coded with enumerations instead.
 */
package object BLAS {

  import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
  import spire.matrix.dense.{Vector, Matrix, PlaneRotation}
  import Sides._
  import Transposition._
  import UpperOrLower._
  import DiagonalProperty._

  /**
   * Operations referencing O(n) elements and performing O(n) flops
   *   - vector scaling
   *   - linear combination of two vectors
   *   - vector dot product and norm
   *   - plane rotations
   */
  trait Level1 {
    /**
     * Scale a vector
     *
     * i.e. x := α x
     */
    def scale(alpha: Double, x: Vector): Unit

    /**
     * Scale a matrix
     *
     * i.e. x := α x
     */
    def scale(alpha: Double, x: Matrix): Unit

    /**
     * Copy x into y
     *
     * i.e. y(0:n) := x(0:n) where n is the dimension of x
     */
    def copy(x: Vector, y: Vector): Unit

    /**
     * Linear combination
     *
     * y := α x + y
     */
    def axpy(alpha: Double, x: Vector, y: Vector): Unit

    /**
     * Linear combination of matrices
     *
     * y := α x + y
     */
    def axpy(alpha: Double, x: Matrix, y: Matrix): Unit

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
     * where G is this rotation and G^T^ its transpose (which is also its
     * inverse).
     */
    def rot(x:Vector, y:Vector, g:PlaneRotation): Unit

    /**
     * Scalar product of x and y
     */
    def dot(x:Vector, y:Vector): Double

    /**
     * The index of the element that has the greatest absolute value
     */
    def idamax(x:Vector): Int
  }

  /**
   * Operations referencing O(n^2^) elements and performing O(n^2^) flops
   *   - product of a matrix and a vector
   *   - rank-1 updates
   */
  trait Level2 {

    /**
     * Product of a general matrix and a vector
     *
     * y := α op(A) x + β y where op(A) = A or A^T^
     *
     * @param trans decides which alternative for op(A) shall be used.
     */
     def gemv(trans: Transposition.Value,
              alpha: Double, a: Matrix,
              x: Vector, beta: Double, y: Vector): Unit

     /**
      * Rank-1 update for a general matrix
      *
      * A := α x y^T^ + A
      */
     def ger(alpha: Double, x: Vector, y: Vector, a: Matrix): Unit

     /**
      * Product of a lower or upper triangular square matrix and a vector
      *
      *   1. x := A x or
      *   2. x := A^T^ x
      *
      * @param trans decides whether to perform 1 or 2
      * @param uplo decides whether A is upper or lower diagonal
      * @param diag decides whether A is unit-diagonal or not
      *
      */
     def trmv(uplo:UpperOrLower.Value, trans:Transposition.Value,
              diag:DiagonalProperty.Value, a:Matrix, x:Vector): Unit
  }

  /**
   * Operations referencing O(n^2^) elements and performing O(n^3^) flops
   *   - multiplication of a pair of matrices, supporting different type for
   *     each of them (general, symmetric, triangular, etc)
   *   - solution of triangularised system of equations for many right-hand
   *     sides
   *
   * Level 3 operations are the critical ones for performance.
   */
  trait Level3 {
    /**
     * Product of two general matrices
     *
     * C := α op(A) op(B) + β C where op(X) = X or X^T^
     *
     * @param transA decides which alternative for op(A) shall be used.
     * @param transB decides which alternative for op(B) shall be used.
     *
     * The matrix C is thus modified in-place whereas matrices A and B are
     * not modified. Result is unpredictable if C overlaps with any of A or B.
     *
     * @todo We could check whether there are such overlaps.
     */
    def gemm(transA:Transposition.Value, transB:Transposition.Value,
             alpha:Double, a:Matrix, b:Matrix,
             beta:Double, c:Matrix): Unit

    protected
    def checkGemmPreconditions(transA:Transposition.Value,
                               transB:Transposition.Value,
                               alpha:Double, a:Matrix, b:Matrix,
                               beta:Double, c:Matrix) {
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
    }

    protected
    def trivialGemm(transA:Transposition.Value, transB:Transposition.Value,
                    a:Matrix, b:Matrix,
                    beta:Double, c:Matrix) {
      import spire.syntax.cfor._
      val (m, n) = c.dimensions
      if(beta == 0)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) = 0 }
      else if(beta != 1)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) *= beta }
      return

    }

    /**
     * Symmetric rank-k updates
     *
     * Either
     *
     *     C := α A A^T + β C (1)
     *
     * or
     *
     *     C := α A^T A + β C (2)
     *
     * where either the upper or lower triangle is updated while the other
     * triangle is not touched.
     *
     * @param trans When its value is Transpose, then (1) is
     * performed, whereas when it is NoTranspose, then (2) is performed.
     *
     * @param uplo When it is Upper (resp. Lower), then only the upper
     * (resp. lower) triangle of C is updated.
     */
    def syrk(uplo:UpperOrLower.Value, trans:Transposition.Value,
             alpha:Double, a:Matrix, beta:Double, c:Matrix): Unit

    /**
     * Solve triangular system of equations with multiple right-hand sides
     *
     * It finds the matrix X such that either
     *
     *  1. op(A) X = α B, or
     *
     *  2. X op(A) = α B,
     *
     * where op(A) = A or A^T^, and X and B are m x n matrices whereas A is a
     * unit, or non-unit, upper or lower triangular matrix.
     *
     * The matrix X is overwritten on B.
     *
     * @param side specifies from which side X is operated upon (thus selecting
     * case 1 or 2)
     *
     * @param uplo specifies whether A is upper or lower diagonal
     *
     * @param trans specifies which alternative op(A) to use
     *
     * @param diag specifies whether A shall be assumed to have a unit diagonal
     */
    def trsm(side:Sides.Value, uplo:UpperOrLower.Value,
             trans:Transposition.Value, unit:DiagonalProperty.Value,
             alpha:Double, a:Matrix, b:Matrix): Unit

    protected
    def checkTrsmPreconditions(side:Sides.Value, uplo:UpperOrLower.Value,
                               trans:Transposition.Value,
                               unit:DiagonalProperty.Value,
                               alpha:Double, a:Matrix, b:Matrix) {
      require(a.isSquare)
      if(side == FromLeft) require(a.dimensions._2 == b.dimensions._1)
      else                 require(b.dimensions._2 == a.dimensions._1)
    }
  }

}
