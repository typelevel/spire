/**
 * Data structures and algorithms for dense matrices.
 *
 * Unless specified otherwise, or not obviously so (e.g. `update`),
 * all methods are pure functions.
 */
package spire.matrix.dense

import spire.implicits._
import spire.math.Complex
import spire.matrix.Constants._
import scala.collection.mutable
import scala.math

/**
 * Common features to all dense matrices
 *
 * - The elements of a matrix are mutable but its dimensions are immutable.
 *
 * - Elements are stored in column-major order and each and every algorithm
 *   in this package assumes so.
 *
 * - A matrix may be viewed as a 2D object but also as a linear sequence,
 *   with its elements ordered by increasing row and column indices,
 *   following the internal column-major layout. Hence this traits extending
 *   IndexedSeq.
 *
 * - This traits is primarily focused on providing flexible and efficient means
 *   of accessing the matrix elements but it also provide a few linear algebra
 *   operations: those for which convenience is more important than performance.
 *   Any performance critical linear algebra should be implemented as a BLAS
 *   package. This includes matrix multiplication, matrix-vector multiplication,
 *   etc.
 *
 * In the following, we will denote by m the number of rows and by n the
 * number of columns, and use the notation A(i:i', j:j') to denote parts
 * of the matrix, with A(:, ...) standing for A(0:m, j) and A(..., :) standing
 * for A(..., 0:n). All intervals k:k' are close at the lower end and open at
 * the upper end.
 *
 * Several methods deal with the k-th diagonal. It is defined as those
 * indices (i,j) such that j-i == k. Thus k = 0 gives the main diagonal,
 * whereas a positive k gives a diagonal above it (superdiagonal),
 * and a negative k gives a diagonal below it (subdiagonal).
 *
 * TODO: parametrize the trait by the type of elements
 */
trait MatrixLike extends mutable.IndexedSeq[Double] {
  protected val m: Int
  protected val n: Int

  /** (number of rows, number of columns) */
  lazy val dimensions = (m,n)

  /** Copies dimensions and elements of this matrix to a new matrix */
  def copyToMatrix = new Matrix(m, n)(toArray)

  /**
    * Copy the upper diagonal part of this matrix (k-th diagonal)
    * and set the remaining elements to 0. If `unitDiagonal` is True,
    * then fill the k-th diagonal with 1's.
    */
  def copyToUpperDiagonal(k:Int = 0, unitDiagonal:Boolean = false): Matrix = {
    val result = Matrix.empty(m, n)
    for(j <- 0 until n) {
      for(i <- 0 to math.min(j-k, m-1)) {
        if(unitDiagonal && i == j) result(j,j) = 1
        else result(i,j) = this(i,j)
      }
    }
    result
  }

  /**
    * Copy the lower diagonal part of this matrix (k-th diagonal)
    * and set the remaining elements to 0. If `unitDiagonal` is True,
    * then fill the k-th diagonal with 1's.
    */
  def copyToLowerDiagonal(k:Int = 0, unitDiagonal:Boolean = false): Matrix = {
    val result = Matrix.empty(m, n)
    for(j <- 0 until n) {
      for(i <- math.max(j-k,0) until m) {
        if(unitDiagonal && i == j) result(j,j) = 1
        else result(i,j) = this(i,j)
      }
    }
    result
  }

  /**
    * Copy the upper Hessenberg part of this matrix
    * and set the remaining elements to 0.
    */
  def copyToUpperHessenberg: Matrix = copyToUpperDiagonal(-1)

  /**
    * Copy the lower Hessenberg part of this matrix
    * and set the remaining elements to 0.
    */
  def copyToLowerHessenberg: Matrix = copyToLowerDiagonal(1)

  /** Set element at row i and column j (indices are 0-based) */
  def update(i:Int, j:Int, value:Double)

  /** Element at row i and column j (indices are 0-based) */
  def apply(i:Int, j:Int): Double

  /** Set k-th element, assuming column-major layout */
  def update(k:Int, value:Double)

  /** k-th element, assuming column-major layout */
  def apply(k:Int): Double

  /** Swap the (i,j) and the (k,l) elements */
  @inline final def swap(i1:Int, j1:Int)(i2:Int, j2:Int): Unit = {
    val k1 = j1 * m + i1
    val k2 = j2 * m + i2
    val tmp = this(k1)
    this(k1) = this(k2)
    this(k2) = tmp
  }

  /** Assign the elements of other to this */
  def := (other:MatrixLike): Unit = {
    cforRange2(0 until dimensions._1, 0 until dimensions._2) { (i,j) =>
      this(i,j) = other(i,j)
    }
  }

  /**
   * Assign the elements of other to this.
   *
   * Precisely, denoting this as A
   * <pre>
   *         [ A(0,0) A(0,1) ] = [ a b ]
   *         [ A(1,0) A(1,1) ]   [ c d ]
   * </pre>
   */
  def := (other:(Double, Double, Double, Double)) {
    this(0, 0) = other._1
    this(0, 1) = other._2
    this(1, 0) = other._3
    this(1, 1) = other._4
  }

  /** Assign the given value to every elements of this */
  def := (e:Double): Unit = {
    cforRange(0 until length) { i => this(i) = e }
  }

  /**
   * Assign the elements produced by the given iterator to this
   *
   * The elements shall be produced by other with a column-major ordering
   */
  def :=(other:Iterator[Double]):Unit = {
    cforRange(0 until length) { i => other.next }
  }

  /**
   * Same matrix as this but with elements rounded to the nearest
   * at the given decimal digit.
   */
  def round(d: Int) = {
    val s = 10 pow d
    new Matrix(m, n)(map((x:Double) => (x*s).round.toDouble/s).toArray)
  }

  /** Total number of elements */
  final def length = m*n

  /** j-th column */
  def column(j:Int) = {
    require(0 <= j && j < n)
    new  MatrixStrides(this, j*m, 1, m)
  }

  /** i-th row */
  def row(i:Int) = {
    require(0 <= i && i < m)
    new  MatrixStrides(this, i, m, n)
  }

  /** k-th diagonal */
  def diagonalOfOrder(k:Int = 0) =
    new MatrixStrides(this,
                      first  = if(k >= 0) (0, k) else (-k, 0),
                      step   = m+1,
                      length = if(k >= 0) math.min(m, n-k)
                               else       math.min(n, m+k))

  /** main diagonal */
  def diagonal = diagonalOfOrder(0)

  /**
   * The open-ended upper end of the sequence of non-zero columns
   *
   * This is equivalent to subroutine ILADLC from LAPACK
   */
  def nonZeroColumnsEndIndex: Int = {
    // Cheaply test the two common cases of an upper- or lower-right
    // non-zero block
    if(this(0, n-1) != 0 || this(m-1, n-1) != 0) n
    else {
      for(j <- n-1 to 0 by -1) {
        for(i <- 0 until m) if(this(i,j) != 0) return j+1
      }
      return 0
    }
  }

  /**
   * The open-ended upper end of the sequence of non-zero rows
   *
   * This is equivalent to subroutine ILADLR from LAPACK
   */
  def nonZeroRowsEndIndex: Int = {
    if(this(m-1, 0) != 0 || this(m-1, n-1) != 0) m
    else {
      var result = -1
      for(j <- 0 until n) {
        var i = m-1
        while(i >= 0 && this(i,j) == 0) i -= 1
        result = math.max(result, i)
      }
      result + 1
    }
  }

  /** Is the matrix square? */
  val isSquare = m == n

  /** Are all elements zero below the k-th diagonal? */
  def isUpperDiagonal(k:Int): Boolean = {
    for(j <- 0 until n) {
      for(i <- math.max(j-k+1, 0) until m) if(this(i,j) != 0) return false
    }
    return true
  }

  /** Are all elements zero above the k-th diagonal? */
  def isLowerDiagonal(k:Int): Boolean = {
    for(j <- 0 until n) {
      for(i <- 0 until math.min(j-k, m)) if(this(i,j) != 0) return false
    }
    return true
  }

  /** Are all elements zero below the main diagonal? */
  def isUpperDiagonal: Boolean = isUpperDiagonal(0)

  /** Are all elements zero above the main diagonal? */
  def isLowerDiagonal: Boolean = isLowerDiagonal(0)

  /** Is the matrix upper Hessenberg? */
  def isUpperHessenberg = isUpperDiagonal(-1)

  /** Is the matrix lower Hessenberg? */
  def isLowerHessenberg = isLowerDiagonal(+1)

  /** Is the matrix diagonal? */
  def isDiagonal: Boolean = {
    if (m != n) return false
    cforRange2(0 until m, 0 until n) { (j,i) =>
      if(i != j && this(i,j) != 0) return false
    }
    return true
  }

  /** Is the matrix zero? */
  def isZero = forall(_ == 0)

  /** Is the matrix the identity matrix */
  def isIdentity: Boolean = {
    if (m != n) return false
    cforRange2(0 until m, 0 until n) { (j,i) =>
      if(this(i,j) != (if(i == j) 1 else 0)) return false
    }
    return true
  }

  /**
   * A rectangular block of this matrix.
   *
   * This returns A(firstRow:endRow, firstColumn:endColumn). If endRow (resp.
   * endColumn) is End, then it takes the value m (resp. n).
   */
  def block(firstRow:Int = 0, endRow:Int = End)
           (firstColumn:Int = 0, endColumn:Int = End) =
    new MatrixBlock(this,
                    firstRow, if(endRow == End) m else endRow,
                    firstColumn, if(endColumn == End) n else endColumn)

  override def equals(other: Any): Boolean =
    other match {
      case that: MatrixLike =>
        m == that.m &&
        n == that.n &&
        (this sameElements that)
      case _ => false
    }

  /** The 1-norm of the matrix, max,,j,, sum,,i,, |a,,ij,,| */
  def norm1: Double = {
    (for(j <- 0 until n) yield column(j).map(_.abs).sum).max
  }

  /** The infinity-norm of the matrix, max,,i,, sum,,j,, |a,,ij,,| */

  def normInf: Double = {
    (for(i <- 0 until m) yield row(i).map(_.abs).sum).max
  }

  /** The Frobenius norm, (∑,,j,, a,,ij,,^2^)^1/2^
    */
  def normFrobenius: Double = math.sqrt(this.map((e) => e*e).sum)

  /** The transpose of this matrix */
  def transposed: Matrix = {
    val result = Matrix.empty(n, m)
    for(i <- 0 until m; j <- 0 until n) result(j,i) = this(i,j)
    result
  }

  /** Sum of the diagonal elements */
  def trace = diagonal.sum

  /** A tabulated display of the matrix
   *
   *  Mostly for debugging purposes.
   */
  override def toString: String = {
    formatted(StringFormatting.elementFormat,
              StringFormatting.useMathematicaFormat)
  }

  def formatted(fmt: String, useMathematicaFormat: Boolean=false): String = {
  val (rowStart, colStep, rowEnd) =
    StringFormatting.ofRows(useMathematicaFormat)
  val (start, rowStep, end) =
    StringFormatting.ofColumns(useMathematicaFormat)
  val disp = (for (i <- 0 until m)
       yield row(i).map(fmt format _).mkString(rowStart, colStep, rowEnd)
    ).mkString(start, rowStep, end)
    StringFormatting.postprocess(disp, useMathematicaFormat)
  }
}

/**
 *  Strides over matrix elements.
 *
 * Note: this is quite inefficient if the underlying matrix is an instance
 * of MatrixBlock but this is not a common use case.
 *
 * @constructor Construct strides starting at the element of index `firstIndex`,
 * advancing by the given `step` at each stride, eventually producing
 * a sequence that has `length` elements. `step` may be negative.
 *
 */
final
class  MatrixStrides(a:MatrixLike,
                     val firstIndex:Int, val step:Int, val length:Int)
  extends VectorLike
{
  val endIndex = firstIndex + length*step

  def this(a:MatrixLike, first:(Int, Int), step:Int, length:Int) =
    this(a, first._2*a.dimensions._1 + first._1, step, length)

  override def iterator = new Iterator[Double] {
    var k = firstIndex
    def hasNext = k < endIndex
    def next = {
      val result = a(k)
      k += step
      result
    }
  }

  def apply(k:Int):Double = a(firstIndex + k*step)

  def update(k:Int, value:Double) = { a(firstIndex + k*step) = value }
}

/**
 * Rectangular block of a matrix that may be considered a matrix itself.
 */
final
class MatrixBlock(private val a:MatrixLike,
                  firstRow:Int, endRow:Int, firstColumn:Int, endColumn:Int)
  extends MatrixLike {

  require(0 <= firstRow  && firstRow <= endRow && endRow <= a.dimensions._1)
  require(0 <= firstColumn  && firstColumn <= endColumn
                                                && endColumn <= a.dimensions._2)

  private val i0 = firstRow
  private val j0 = firstColumn
  private val k0 = j0*a.dimensions._1 + i0
  protected val m = endRow - firstRow
  protected val n = endColumn - firstColumn

  /** Top-left corner of this block */
  def topLeftCorner = (i0, j0)

  /**
   * Set element at row i and column j (indices are 0-based)
   *
   * This implements MatrixLike abstract function
   */
  def update(i:Int, j:Int, value:Double) = {
    a(k0 + j*a.dimensions._1 + i) = value
  }

  /**
   * Element at row i and column j (indices are 0-based)
   *
   * This implements MatrixLike abstract method.
   */
  def apply(i:Int, j:Int): Double = a(k0 + j*a.dimensions._1 + i)

  /**
   * Set k-th element, assuming column-major layout
   *
   * This method is inefficient.
   *
   * This implements MatrixLike abstract method.
   */
  def update(k:Int, value:Double) { this(k%m, k/m) = value }

  /** k-th element, assuming column-major layout
   *
   * This method is inefficient.
   *
   * This implements MatrixLike abstract method.
   */
  def apply(k:Int) = this(k%m, k/m)
}


/**
 * Matrix whose dimensions are set at runtime.
 *
 * We accept null dimensions although it makes little sense so as to have
 * the same behaviour as MatrixBlock for which zero dimensions make sense
 * (empty block).
 *
 * TODO: parametrize the class by the type of elements
 *
 * @constructor Create a m x n matrix.
 * Synopsis:
 * {{{
 * // Create a zero m x n matrix
 * val a = new Matrix(m, n)
 * // or more clearly
 * val b = Matrix.zero(m,n)
 *
 * // Create a m x n matrix with the given elements
 * // that come stored in column-major order
 * val a = new Matrix(m, n, elements)
 *
 * // Create a m x n matrix with the given elements
 * // that come stored in row-major order
 * val a = new Matrix(m, n, elements, given_in_row_major=true)
 * }}}
 */
class Matrix(val m: Int, val n: Int)
            (val elems: Array[Double] = new Array[Double](m*n))
extends MatrixLike {
  require(m >= 0)
  require(n >= 0)
  require(elems.length == m * n)

  /**
   * Overriden for efficiency
   */
  override def iterator = elems.iterator

  /**
   * Overloaded for efficiency.
   */
  def sameElements(other: Matrix) = elems === other.elems

  /**
   * Permute the elements to change the ordering from row- to column-major.
   * This is equivalent to transposing the matrix in-place.
   *
   * This is the algorithm described in [1], taking advantage of Remark 1.
   * Note that the author credits P.F. Windley for this algorithm (ref 1.)
   *
   * [1] Correctness proof of an in-place permutation.
   *     A. J. W. Duijvestijn.
   *     BIT, 1972 vol. 12 pp. 318-324.
  */
  def permuteFromRowMajorToColumnMajor() = {
    /* In term of transposition, the target is a matrix of dimension (n,m),
       hence the fact that m and n are swapped compared to [1] */
    cforRange(1 until length - 2) { k =>
      var kn = (k % m)*n + k/m
      while(kn < k) kn = (kn % m)*n + kn/m

      if(kn != k) {
        val t = elems(kn)
        elems(kn) = elems(k)
        elems(k) = t
      }
    }
  }

  /**
   * Set element at row i and column j (indices are 0-based)
   *
   * This implements MatrixLike abstract method
   */
  final def update(i: Int, j: Int, value: Double) = { elems(j*m + i) = value }

  /**
   * Element at row i and column j (indices are 0-based)
   *
   * This implements MatrixLike abstract method
   */
  final def apply(i: Int, j: Int): Double = elems(j*m + i)

  /** Set k-th element, assuming column-major layout
    *
    * This implements MatrixLike abstract method.
    */
  final def update(k:Int, value:Double) { elems(k) = value }

  /** k-th element, assuming column-major layout
    *
    * This implements MatrixLike abstract method.
    */
  final def apply(k:Int) = elems(k)

  /**
   * The eigenvalues of this matrix
   *
   * If inplace is true, then this matrix elements are overwritten by
   * essentially garbage from the point of view of the client code.
   * This option is provided to deal more efficiently with huge matrices.
   */
  def eigenvalues(inplace:Boolean=false):Array[Complex[Double]] = {
    val HessenbergDecomposition = Hessenberg.DecompositionWithNaiveBLAS
    val SchurDecomposition = Schur.RealDecompositionWithDoubleShiftQRAlgorithm
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(dimensions._1))
    val a = if(inplace) this else copyToMatrix
    val hd = HessenbergDecomposition.withUnblockedAlgorithm(a)()
    val sd = SchurDecomposition(a, None, fullSchurFormWanted=false)()()
    for((a, b) <- sd.eigenvalues) yield Complex(a, b)
  }

}

/** Matrix companion object */
object Matrix {

  /** Create a m x n matrix with the given elements listed in row-major order */
  def apply(m: Int, n: Int)(elems: Double*): Matrix = {
    val matrix = new Matrix(m, n)(elems.toArray)
    matrix.permuteFromRowMajorToColumnMajor()
    matrix
  }

  /** Create a m x n matrix whose (i,j) elements is f(i,j) */
  def tabulate(m:Int, n:Int)(f: (Int,Int) => Double) = {
    val matrix = empty(m, n)
    cforRange2(0 until m, 0 until n) { (i,j) => matrix(i,j) = f(i,j) }
    matrix
  }

  /** Create the identity matrix of dimension m */
  def identity(m: Int): Matrix = {
    val arr = new Array[Double](m * m)
    cforRange(0 until arr.length by m+1) { i => arr(i) = 1.0 }
    new Matrix(m, m)(arr)
  }

  /**
   * Create an m x n matrix with uninitialised elements
   *
   * Actually, the elements are currently initialised to zero
   * but it would be nice to find a way to work that around (TODO).
   */
  def empty(m:Int, n:Int): Matrix = zero(m, n)

  /** Create an n x n matrix with uninitialised elements */
  def empty(n:Int): Matrix = empty(n, n)

  /** Create the zero matrix of dimension m x n */
  def zero(m:Int, n:Int): Matrix = new Matrix(m, n)()

  /** Create the zero matrix of dimension n x n */
  def zero(n:Int): Matrix = zero(n, n)

  /**
   * Create a matrix from the given string.
   * The format is:
   *
   * {{{
   * [ x x x x ]
   * [ x x x x ]
   * ........
   * [ x x x x ]
   * }}}
   *
   * where each `x` is a number.
   */
  def fromString(s: String): Matrix = {
    val lines = s.trim.split("\n")
    val rows = lines.map { line =>
      if (!line.startsWith("[") || !line.endsWith("]"))
        throw new IllegalArgumentException()
      val data = line.substring(1, line.length - 1)
      data.split(" +").map(_.toDouble).toArray
    }
    val n = rows.length
    if (n < 1) throw new IllegalArgumentException()
    val m = rows(0).length
    rows.foreach { row =>
      if (row.length != m) throw new IllegalArgumentException()
    }
    val arr = new Array[Double](m * n)
    cforRange2(0 until n, 0 until m) { (j, i) =>
      arr(j * m + i) = rows(j)(i)
    }
    new Matrix(m, n)(arr)
  }
}
