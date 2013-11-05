package spire.matrix.dense

import spire.matrix.Transposition._

import java.lang.Math.copySign
import spire.matrix.NumericPropertiesOfDouble

import spire.syntax.cfor._

/**
 * Elementary Reflector
 *
 * We use LAPACK conventions [1].
 *
 * An elementary reflector of order n is a n x n orthogonal matrix of the form
 *
 *     H = I - τ v v^T^
 *
 * where τ is a scalar and v is a vector of size n. This is also named
 * a Householder matrix (not because Householder invented the concept, which
 * had been known for about a century, but because he found new and interesting
 * ways to use them). We will always assume that v(0) = 1 and therefore we will
 * only require the knowledge of v(1:n), the so-called essential part of the
 * reflector [2].
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 *
 * [2] LAPACK only ever constructs reflectors with v(0) = 1 but the subroutine
 * applying the reflectors to matrices do not assume that, forcing the client
 * code using them to set v(0) = 1 by hand, and eventually to restore v(0) to
 * its original value because v always share storage with a matrix row or column
 * for which v(0) aliases an element that is part of the desired output. We
 * prefer to avoid this ugly hack altogether.
 */
trait ElementaryReflectorLike extends BLAS.Level2 with BLAS.Level1 {

  val tau:Double

  /**
   * The essential part.
   *
   * It is required to have a defined value only if tau is non-zero
   */
  val essentialPart:VectorLike

  def isIdentity = tau == 0

  override def toString =
    if(isIdentity) "Identity"
    else "I - tau v v^T with tau=%f and v^T=%s" format (tau, essentialPart)

  /**
   * The position after which the vector v has only zero elements.
   *
   * i.e. if
   * <pre>
   *   v = [ 1      ]
   *       [ v(1)   ]
   *       [ ...    ]
   *       [ v(p-1) ]
   *       [ 0      ]
   *       [ ...    ]
   *       [ 0      ]
   * </pre>
   *
   * then this is p
   */
  val vectorNonZeroEndIndex: Int =
    essentialPart.lastIndexWhere(_ != 0) + 2

  /**
   * Apply H on the left of C, in-place
   *
   * i.e. C := H C
   *
   * @param work needs to be at least as large as the number of columns of C
   *
   * Reference: subroutine DLARF in LAPACK [1] (our implementation
   * differs because we do not have the whole vector of the reflector)
   */
  def applyOnLeft(c:MatrixLike)(implicit work:Scratchpad) {
    /**
     * Decompose our vector v as
     *
     *       [ 1      ]
     *   v = [ v(1:p) ]
     *       [ 0      ]
     *
     * and accordingly
     *
     *       [ C(0  , 0:q) 0 ]
     *   C = [ C(1:p, 0:q) 0 ]
     *       [      ~      ~ ]
     *
     * then w = C^T v reads
     *
     *   w = [ C(0, 0:q)^T + C(1:p, 0:q)^T v(1:p) ]
     *       [            0                       ]
     *
     * and finally H C = C - tau v w^T where
     *
     *           [ w(0:q)^T          0 ]
     *   v w^T = [ v(1:p) w(0:q)^T   0 ]
     *           [       0           0 ]
     *
     * So we only need to apply a rank-1 update to C(0:p, 0:q),
     * leaving the rest of C untouched.
     */
    if(tau != 0) {
      val (m,n) = c.dimensions
      val p = vectorNonZeroEndIndex
      val q = c.block(1,p)(0,n).nonZeroColumnsEndIndex
      val w = work.vector.block(0,q)
      val cb = c.block(1,p)(0,q)
      val cr = c.row(0).block(0,q)
      val vb = essentialPart.block(0,p-1)
      copy(cr, w)
      gemv(Transpose, alpha=1.0, cb, vb, beta=1.0, w)
      axpy(-tau, w, cr)
      ger(-tau, vb, w, cb)
    }
  }

  /**
   * Apply H on the right of C, in-place
   *
   * i.e. C := C H
   *
   * @param work needs to be at least as large as the number of rows of C
   *
   * Reference: subroutine DLARF in LAPACK [1] (our implementation
   * differs because we do not have the whole vector of the reflector)
   */
  def applyOnRight(c:MatrixLike)(implicit work:Scratchpad) {
    /**
     * Decompose our vector v as
     *
     *       [ 1      ]
     *   v = [ v(1:q) ]
     *       [ 0      ]
     *
     * and accordingly
     *
     *   C = [ C(0:p, 0) C(0:p, 1:q) ~ ]
     *       [     0         0       ~ ]
     *
     * then w = C v reads
     *
     *   w = [ C(0:p, 0) + C(0:p, 1:q) v(1:q) ]
     *       [            0                   ]
     *
     * and finally H C = C - tau w v^T where
     *
     *   w v^T = [ w(0:p) w(0:p) v(1:q)^T   0 ]
     *           [ 0      0                 0 ]
     *
     * So we only need to apply a rank-1 update to C(0:p, 0:q),
     * leaving the rest of C untouched.
     */
    if(tau != 0) {
      val (m,n) = c.dimensions
      val q = vectorNonZeroEndIndex
      val p = c.block(0,m)(1,q).nonZeroRowsEndIndex
      val w = work.vector.block(0,p)
      val cb = c.block(0,p)(1,q)
      val cc = c.column(0).block(0,p)
      val vb = essentialPart.block(0,q-1)
      copy(cc, w)
      gemv(NoTranspose, alpha=1.0, cb, vb, beta=1.0, w)
      axpy(-tau, w, cc)
      ger(-tau, w, vb, cb)
    }
  }
}

/**
 * Elementary reflector companion object to be.
 */
trait ElementaryReflectorLikeCompanion
  extends BLAS.Level1
  with EuclideanNorm with NumericPropertiesOfDouble
{
  val safeMin = safeMinimum/epsilonMachine
  val safeMinInv = 1.0/safeMin

  /** Factory method */
  def apply(tau:Double, v:VectorLike): ElementaryReflectorLike

  /**
   * Construct a reflector whose application to y zeroes all but y(0)
   *
   * We follow the conventions and notations of LAPACK [1].
   *
   * Given a vector
   * <pre>
   *   y = [ α ]
   *       [ x ]
   * </pre>
   * of size n, where α is a scalar, and therefore x is a vector of size n-1,
   * we seek the elementary reflector H such that
   * <pre>
   *      H [ α ] = [ β ]
   *        [ x ]   [ 0 ]
   * </pre>
   *
   * where β is a scalar. The solution is represented as
   * <pre>
   *     H = I - τ [ 1 ] [ 1 v^T^ ]
   *               [ v ]
   * </pre>
   *
   * where τ is a scalar and v is a vector of size n-1 called
   * the essential part of the elementary reflector.
   *
   * If x=0, then τ=0 and therefore H=I, otherwise 1 ≤ τ ≤ 2.
   *
   * The construction is done in-place, leaving y(0) = β and y(1:n) = v
   *
   * Reference: subroutine DLARFG in LAPACK [1]
   *
   * [1] LAPACK Users' Guide.
   *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
   *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
   *     and D Sorensen.
   *     Society for Industrial and Applied Mathematics,
   *     Philadelphia, PA, Third.
   */
  def annihilateAndConstruct(y:VectorLike): ElementaryReflectorLike = {
    if(y.length <= 1) this(0, Vector.empty(0))
    else {
      var x = y.block(1, y.length)
      var xNorm = euclideanNorm(x)
      if(xNorm == 0) this(0, x)
      else { // Non-trivial case
        var count = 0
        var alpha = y(0)
        var beta = -copySign(euclideanNorm2D(alpha, xNorm), alpha)
        // If beta is that small, ||x|| is even smaller, and the both of them
        // may be inaccurate. Thus ...
        if(beta.abs < safeMin) {
          // .. rescale x, beta and alpha until beta is accurate enough...
          do {
            count += 1
            scale(safeMinInv, x)
            beta *= safeMinInv
            alpha *= safeMinInv
          } while(beta.abs < safeMin)

          // ... and now that safeMin <= beta <= 1, recompute ||x|| and beta
          xNorm = euclideanNorm(x)
          beta = -copySign(euclideanNorm2D(alpha, xNorm), alpha)
        }

        // Compute tau and v now that we have an accurate beta
        val tau = (beta - alpha)/beta
        scale(1.0/(alpha - beta), x)

        // Comment verbatim from LAPACK code:
        // if alpha is subnormal, it may lose relative accuracy
        cforRange(1 to count) { k => beta *= safeMin }

        // get results out
        y(0) = beta
        this(tau, x)
      }
    }
  }
}


class ElementaryReflectorWithNaiveBLAS(val tau:Double,
                                       val essentialPart:VectorLike)
extends ElementaryReflectorLike with BLAS.NaiveLevel1 with BLAS.NaiveLevel2

object ElementaryReflectorWithNaiveBLAS
extends ElementaryReflectorLikeCompanion
with BLAS.NaiveLevel1 with BLAS.NaiveLevel2 {
  def apply(tau:Double, v:VectorLike) =
    new ElementaryReflectorWithNaiveBLAS(tau, v)
}

/**
 * Elementary reflector of dimension n = 2 or 3
 *
 * The essential part is (v1, v2) or just (v1) if v2 is None.
 * This is a hand-optimised version of ElementaryReflector, that is primary
 * useful for the bulge chasing of the real Schur decomposition.
 */
final class TinyElementaryReflector private[this](tau:Double,
                                                  val n:Int,
                                                  val v1:Double, val v2:Double)
{
  val t0 = tau
  val t1 = tau*v1
  val t2 = if(n == 3) tau*v2 else 0

  /** Construct an elementary reflector of dimension 2 */
  def this(tau:Double, v1:Double) = this(tau, 2, v1, 0)

  /** Construct an elementary reflector of dimension 3 */
  def this(tau:Double, v1:Double, v2:Double) = this(tau, 3, v1, v2)

  /**
   * Denoting by H this reflector, perform C(i:i+n, j1:j2) := H C(i:i+n, j1:j2)
   * where i = startingRow and (j1, j2) = columns
   */
  def applyOnLeft(c:MatrixLike)(startingRow:Int,
                                columns:(Int, Int)=(0, c.dimensions._2)) {
    val i = startingRow
    val (j1, j2) = columns
    cforRange(j1 until j2) { j =>
      var s = c(i, j) + v1*c(i+1, j)
      if(n == 3) s += v2*c(i+2, j)
      c(i, j) -= t0*s
      c(i+1, j) -= t1*s
      if(n == 3) c(i+2, j) -= t2*s
    }
  }

  /**
   * Denoting by H this reflector, perform C(i1:i2, j:j+n) := C(i1:i2, j:j+n) H
   * where j = startingColumn and (i1, i2) = rows
   */
  def applyOnRight(c:MatrixLike)(startingColumn:Int,
                                 rows:(Int, Int)=(0, c.dimensions._1)) {
    val (i1, i2) = rows
    val j = startingColumn
    cforRange(i1 until i2) { i =>
      var s = c(i, j) + c(i, j+1)*v1
      if(n == 3) s += c(i, j+2)*v2
      c(i, j) -= t0*s
      c(i, j+1) -= t1*s
      if(n == 3) c(i, j+2) -= t2*s
    }
  }
}

object TinyElementaryReflector {

    def annihilateAndConstruct(y:VectorLike) = {
      require(y.length == 2 || y.length == 3)
      val h = ElementaryReflectorWithNaiveBLAS.annihilateAndConstruct(y)
      if(y.length == 2)
        new TinyElementaryReflector(h.tau,
                                    h.essentialPart(0))
      else
        new TinyElementaryReflector(h.tau,
                                    h.essentialPart(0), h.essentialPart(1))
    }
}
