/**
 * Data structures and algorithms for dense matrices.
 *
 * Unless specified otherwise, or not obviously so (e.g. `update`),
 * all methods are pure functions.
 */
package spire.matrix.dense

import spire.implicits._
import spire.math.Complex
import spire.matrix.Transposition
import scala.math

/**
 * Dense matrix whose dimensions are set at runtime.
 *
 * - The elements of a matrix are mutable but its dimensions are immutable.
 *
 * - Elements are stored in column-major order and each and every algorithm
 *   in this package assumes so.
 *
 * - This traits is primarily focused on providing flexible and efficient means
 *   of accessing the matrix elements but it also provide a few linear algebra
 *   operations: those for which convenience is more important than performance.
 *   Any performance critical linear algebra should be implemented in the BLAS
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
 * TODO: parametrize by the type of elements
 */
class Matrix(val m:Int, val n:Int, val ld:Int,
             val start:Int, val elements:Array[Double])
extends Iterable[Double] {
  require(m >= 0)
  require(n >= 0)
  require(m <= ld)
  require(start >= 0)
  require(start + m + (n-1)*ld <= elements.length)

  def this(m:Int, n:Int, elements:Array[Double]) = this(m, n, m, 0, elements)

  /** (number of rows, number of columns) */
  lazy val dimensions = (m,n)

  /**
   * Set element at row i and column j (indices are 0-based)
   *
   * This implements Matrix abstract method
   */
  final def update(i: Int, j: Int, x: Double) = {
    elements(start + i + j*ld) = x
  }

  /**
   * Element at row i and column j (indices are 0-based)
   *
   * This implements Matrix abstract method
   */
  final def apply(i: Int, j: Int): Double = elements(start + i + j*ld)

  final def block(firstRow:Int, endRow:Int)(firstColumn:Int, endColumn:Int) =
    new Matrix(endRow - firstRow, endColumn - firstColumn, ld,
               start + firstRow + firstColumn*ld, elements)

  final
  def column(j:Int) = new Vector(m, 1, start + j*ld, elements)

  final
  def row(i:Int) = new Vector(n, ld, start + i, elements)

  /** Swap the (i,j) and the (k,l) elements */
  final
  def swap(i1:Int, j1:Int)(i2:Int, j2:Int): Unit = {
    val k1 = start + j1 * ld + i1
    val k2 = start + j2 * ld + i2
    val tmp = elements(k1)
    elements(k1) = elements(k2)
    elements(k2) = tmp
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Matrix =>
        m == that.m &&
        n == that.n &&
        (this sameElements that)
      case _ => false
    }

  /** Whether this has the same elements as other */
  def sameElements(that:Matrix): Boolean = {
    cforRange2(0 until n, 0 until m) { (j,i) =>
      if(this(i,j) != that(i,j)) return false
    }
    return true
  }

  /** Traversal in column-major order */
  def iterator =
    if(m == ld)
      new Iterator[Double] {
        var l = start
        def hasNext = l < start + m*n
        def next = {
          val elt = elements(l)
          l += 1
          elt
        }
      }
    else
      new Iterator[Double] {
        var l = start
        var i = 0
        def hasNext = l < start + m + ld*(n-1)
        def next = {
          if(i == m) {
            i = 0
            l += ld - m
          }
          val elt = elements(l)
          i += 1
          l += 1
          elt
        }
      }

  /** Assign the elements of other to this */
  def := (other:Matrix): Unit = {
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
    cforRange2(0 until n, 0 until m) { (j,i) => this(i,j) = e }
  }

  /**
   * Assign the elements produced by the given iterator to this
   *
   * The elements shall be produced by other with a column-major ordering
   */
  def :=(other:Iterator[Double]):Unit = {
    cforRange2(0 until n, 0 until m) { (j,i) => this(i,j) = other.next }
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

  /** Is the matrix square? */
  def isSquare = m == n

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

  /** Copies dimensions and elements of this matrix to a new matrix */
  def copyToMatrix = new Matrix(m, n, m, 0, toArray)

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

  /**
   * Same matrix as this but with elements rounded to the nearest
   * at the given decimal digit.
   */
  def round(d: Int) = {
    val s = 10 pow d
    new Matrix(m, n, ld, 0, map((x:Double) => (x*s).round.toDouble/s).toArray)
  }

  /** k-th diagonal */
  def diagonalOfOrder(k:Int = 0) = {
    if(k >= 0) new Vector(math.min(m, n-k), ld+1, k*ld, elements)
    else       new Vector(math.min(n, m+k), ld+1, -k  , elements)
  }

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
      cforRange(n-1 to 0 by -1) { j =>
        cforRange(0 until m) { i => if(this(i,j) != 0) return j+1 }
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
      cforRange(0 until n) { j =>
        var i = m-1
        while(i >= 0 && this(i,j) == 0) i -= 1
        result = math.max(result, i)
      }
      result + 1
    }
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

  /**
   * The eigenvalues of this matrix
   *
   * If inplace is true, then this matrix elements are overwritten by
   * essentially garbage from the point of view of the client code.
   * This option is provided to deal more efficiently with huge matrices.
   */
  def eigenvalues(inplace:Boolean=false):Array[Complex[Double]] = {
    val n = dimensions._1
    val HessenbergDecomposition = Hessenberg.DecompositionWithNaiveBLAS
    val SchurDecomposition = Schur.RealDecompositionWithDoubleShiftQRAlgorithm
    implicit val work = new Scratchpad(
      HessenbergDecomposition.unblockedMinimumScratchpad(n))
    val a = if(inplace) this else copyToMatrix
    val hd = HessenbergDecomposition.withUnblockedAlgorithm(a)()
    val sd = SchurDecomposition(a, None, fullSchurFormWanted=false)()()
    val buf = new scala.collection.mutable.ArrayBuffer[Complex[Double]](n)
    cforRange(0 until n) { i => buf += Complex(sd.eigenRe(i), sd.eigenIm(i))}
    buf.toArray
  }

  /**
   * Solve a system of equations
   *
   * It solves the equations A x,,j,, = b,,j,, for 0 <= j < p where A is this
   * matrix and b,,j,, is the j-th column of the matrix B, i.e. this actually
   * solves the equation A X = B for the matrix X.
   *
   * @overwriteB specifies whether B is overwritten by X, in which case
   *             the returned value is the same object as B
   * @overwriteA specifies whether A shall be overwritten in the process of
   *             solving the equations
   */
  def solve(b:Matrix,
            overwriteA:Boolean = false, overwriteB:Boolean = false) = {
    val a = if(overwriteA) this else copyToMatrix
    val lu = LU.RecursiveDecompositionConstructionWithNaiveBLAS(a)
    val x = if(overwriteB) b else b.copyToMatrix
    lu.solve(Transposition.NoTranspose, x)
    x
  }
}

/** Construction of matrices and other utilities */
trait MatrixConstruction[M <: Matrix] {

  /**
   * Create an m x n matrix with uninitialised elements
   */
  def empty(m:Int, n:Int): M

  /**
   * Create a m x n matrix with the given elements listed in column-major order
   */
  def apply(m:Int, n:Int, elements:Array[Double]): M

  /**
   * Create a m x n matrix with the given elements listed in row-major order
   */
  def apply(m: Int, n: Int)(elements: Double*): M = {
    require(m*n == elements.size)
    val elems = elements.toArray
    permuteFromRowMajorToColumnMajor(m, n, elems)
    this(m, n, elems)
  }

  /**
   * On input, elements shall be viewed as a m x n matrix stored in row-major
   * order. On output, elements has been permuted in-place so as to store
   * the same m x n matrix stored in column-major order.
   *
   * This is the algorithm described in [1], taking advantage of Remark 1.
   * Note that the author credits P.F. Windley for this algorithm (ref 1.)
   *
   * [1] Correctness proof of an in-place permutation.
   *     A. J. W. Duijvestijn.
   *     BIT, 1972 vol. 12 pp. 318-324.
  */
  def permuteFromRowMajorToColumnMajor(m:Int, n:Int, elements:Array[Double]) = {
    /* In term of transposition, the target is a matrix of dimension (n,m),
       hence the fact that m and n are swapped compared to [1] */
    cforRange(1 until m*n - 2) { k =>
      var kn = (k % m)*n + k/m
      while(kn < k) kn = (kn % m)*n + kn/m

      if(kn != k) {
        val t = elements(kn)
        elements(kn) = elements(k)
        elements(k) = t
      }
    }
  }

  /** Create the zero matrix of dimension m x n */
  def zero(m:Int, n:Int) = this(m, n, new Array[Double](m*n))

  /** Create an n x n matrix with uninitialised elements */
  def empty(n:Int): M = empty(n, n)

  /** Create the zero matrix of dimension n x n */
  def zero(n:Int): M = zero(n, n)

  /** Create the identity matrix of dimension m */
  def identity(m: Int): M = {
    val matrix = zero(m,m)
    matrix.diagonal := 1.0
    matrix
  }

  /** Create a m x n matrix whose (i,j) elements is f(i,j) */
  def tabulate(m:Int, n:Int)(f: (Int,Int) => Double) = {
    val matrix = empty(m, n)
    cforRange2(0 until n, 0 until m) { (j,i) => matrix(i,j) = f(i,j) }
    matrix
  }

  /**
   * Create a m x n matrix whose elements are obtained in column-major order
   * by repeateadly evaluating the given expression.
   */
  def fill(m:Int, n:Int)(element: => Double) = {
    val matrix = empty(m,n)
    cforRange2(0 until n, 0 until m) { (j,i) => matrix(i,j) = element }
    matrix
  }

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
  def fromString(s: String): M = {
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
    this(m, n, arr)
  }
}

object Matrix extends MatrixConstruction[Matrix] {

  /**
   * Actually, the elements are currently initialised to zero
   * but it would be nice to find a way to work that around (TODO).
   */
  def empty(m:Int, n:Int): Matrix = zero(m, n)

  def apply(m:Int, n:Int, elements:Array[Double]) =
    new Matrix(m, n, m, 0, elements)
}


