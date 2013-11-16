package spire.matrix.dense

import spire.syntax.cfor._

/**
 * A permutation of the subset {r, ..., s-1} of the set of integers {0, ..., n-1}
 *
 * The permutation is decomposed into a product of transpositions
 * (ip, jp) ... (i1, j1)(i0, j0) with s > i0 > i1 > ... > ip >= r
 * (transpositions are applied in right-to-left order) and it is then stored
 * in an array p such that p(i0) = j0, p(i1) = j1, ..., p(ip) = jp and p(k) = k
 * for any integer k in {0, ..., n-1} that is not in the set (i0, i1, ..., ip).
 *
 */
class Permutation(private val p:Array[Int],
                  private val reverse:Boolean = false)
                 (val r:Int=0, val s:Int=p.length) {
  require(0 <= r && r < s && s <= p.length)

  def dimension = s - r

  /** Read access to the array p described above */
  def apply(i:Int) = p(r + i) - r

  /** Write access to the arry p described above */
  def update(i:Int, j:Int) { p(r + i) = r + j }

  def toList = p.toList

  /**
   * Inverse of this, in the sense of permutation group structure
   *
   * It shares the same array p for efficiency
   */
  def inverse = new Permutation(p, !reverse)(r, s)

  override def toString =
    (for(i <- (range._1 until range._3 by range._2).iterator) yield p(i)
    ).mkString("( ", " ", " )")

  private def range = if(reverse) (dimension-1, -1, -1)
                      else        (0, +1, dimension)

  /**
   * A permutation of the new subset {r', ..., s'-1} of {0, ..., n-1}
   *
   * It shares the same array p for efficiency
   */
  def subset(rp:Int, sp:Int) = new Permutation(p, reverse)(rp, sp)

  /**
   * Permute the rows of the matrix a
   *
   * @blockSize the algorithm proceeds by block of r columns where r is given
   *            by this argument
   */
  def permute_rows(a:MatrixLike, blockSize:Int = 32) {
    require(a.dimensions._1 == dimension)
    val (m,n) = a.dimensions
    val (startRow, stepRow, endRow) = range
    val endBlocks = (n/blockSize)*blockSize
    val endColBlocks = (n/blockSize)*blockSize

    cforRange(0 until endColBlocks by blockSize) { j =>
      cfor(startRow)(_ != endRow, _ + stepRow) { i =>
        val ip = this(i)
        if(ip != i) {
          cforRange(j until j + blockSize) { k => a.swap(ip,k)(i,k) }
        }
      }
    }
    if(endColBlocks != n) {
      cfor(startRow)(_ != endRow, _ + stepRow) { i =>
        val ip = this(i)
        if(ip != i) {
          cforRange(endColBlocks until n) { k => a.swap(ip,k)(i,k) }
        }
      }
    }
  }
}

object Permutation {

  def apply(elems:Int*) = new Permutation(elems.toArray)()

  def identity(n:Int) = new Permutation(Array.tabulate[Int](n)(i => i))()
}
