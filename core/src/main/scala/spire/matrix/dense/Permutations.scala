package spire.matrix.dense

import spire.syntax.cfor._

/**
 * Permutation of the integer range "0 until n"
 * that leaves the subrange "r until s" invariant.
 *
 * We start with a permutation P decomposed into a product of transpositions
 * (n-1, p(n-1)) (n-2, p(n-2)) ... (1, p(1)) (0, p(0)),
 * where those transpositions are applied in right-to-left order, which is such
 * that r <= p(i) < s if r <= i < s. This class then represents the
 * restriction Q of P to the range "r until s" and it is then viewed as a
 * permutation of the range "0 until r-s" decomposed in the product of
 * transpositions
 * (r-s-1, p(s-1)) (r-s-2, p(s-2)) ... (1, p(r+1)) (0, p(r)).
 */
class Permutation private (private val p:Array[Int],
                           private val reverse:Boolean = false)
                          (private val r:Int=0, private val s:Int=p.length) {
  require(0 <= r && r < s && s <= p.length)

  /** The permutation is viewed as one of the range 0 until dimension */
  def dimension = s - r

  /**
   * (i, this(i)), for 0 <= i < dimension is the i-th transposition
   * of the product Q, counting from the right
   */
  def apply(i:Int) = {
    require(0 <= i && i < dimension)
    p(r + i) - r
  }

  /**
   * Set the i-th transposition, for 0 <= i < dimension of the
   * product Q to (i, j), counting from the right
   */
  def update(i:Int, j:Int) {
    require(0 <= i && i < dimension)
    require(0 <= j && j < dimension)
    p(r + i) = r + j
  }

  /**
   * Inverse of this, in the sense of permutation group structure
   *
   * It shares the same internal state as this for efficiency.
   */
  def inverse = new Permutation(p, !reverse)(r, s)

  /**
   * The restriction of Q to the subrange r1 until s1
   *
   * It shares the same internal state as this for a semantic reason:
   * it allows the construction of a partition by pieces without the code
   * dealing with one piece needing to know about anything else than the
   * integer range that piece operates upon.
   */
  def restrictTo(r1:Int, s1:Int) = new Permutation(p, reverse)(r+r1, r+s1)

  /** Textual representation of Q as a permutation of 0 until dimension */
  override def toString = {
    val range = if(!reverse) 0 until dimension else dimension - 1 to 0 by -1
    range.map(i => s"($i, ${this(i)})").mkString(", ")
  }

  /** The list p(0) :: p(1) :: ... :: p(dimension-1) :: Nil */
  def toList = p.slice(r,s).toList

  /**
   * Permute the rows of the matrix a
   *
   * @blockSize the algorithm proceeds by column block of that given width
   */
  def permute_rows(a:Matrix, blockSize:Int = 32) {
    require(a.dimensions._1 == dimension)
    val (m,n) = a.dimensions
    val (startRow, testRow, incRow) =
      if(!reverse) (0          , (i:Int) => i < dimension, (i:Int) => i + 1)
      else         (dimension-1, (i:Int) => i >= 0       , (i:Int) => i - 1)
    val endColBlocks = (n/blockSize)*blockSize

    cfor(0)(_ < endColBlocks, _ + blockSize) { j =>
      cfor(startRow)(testRow, incRow) { i =>
        val ip = this(i)
        if(ip != i) {
          cforRange(j until j + blockSize) { k => a.swap(ip,k)(i,k) }
        }
      }
    }
    if(endColBlocks != n) {
      cfor(startRow)(testRow, incRow) { i =>
        val ip = this(i)
        if(ip != i) {
          cforRange(endColBlocks until n) { k => a.swap(ip,k)(i,k) }
        }
      }
    }
  }
}

object Permutation {

  /**
   * Construct the product of transpositions
   * (n-1, p(n-1)) ... (1, p(1)) (0, p(0))
   * where n = p.length
   */
  def apply(p:Array[Int]):Permutation = new Permutation(p)()

  /** Convenient overload */
  def apply(p:Int*):Permutation = apply(p.toArray)

  /** Construct the identical permutation of the range 0 until n */
  def identity(n:Int) = new Permutation(Array.tabulate[Int](n)(i => i))()
}
