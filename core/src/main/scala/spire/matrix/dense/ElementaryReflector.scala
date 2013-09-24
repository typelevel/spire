package spire.matrix.dense

import spire.matrix.BLAS
import spire.matrix.Transposition._

import java.lang.Math.copySign
import spire.matrix.NumericPropertiesOfDouble

/**
 * Elementary Reflector
 *
 * We use LAPACK conventions [1].
 *
 * An elementary reflector of order n is a n x n matrix of the form
 * \[
 *     H = I - \tau v v^T
 * \]
 * where $\tau$ is a scalar and $v$ is a vector of size n. This is also named
 * a Householder matrix (not because Householder invented the concept, which
 * had been known for about a century, but because he found new and interesting
 * ways to use them). We will always assume that v(0) = 1 and therefore we will
 * only require the knowledge of v(1:n), the so-called essential part of the
 * reflector [2].
 *
 *
 * The latter variant nicely factor out the pattern used throughout LAPACK
 * that consists in setting to 1 the matrix element corresponding to v(0),
 * then applying the reflector, and finally restoring the original value of
 * that matrix element.
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
trait ElementaryReflectorLike
extends BLAS.level2.Interface with BLAS.level1.Interface {

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
   * i.e. if $v = (1 v_1 \cdots v_{p-1} 0 \cdots 0)^T$,
   * then this is p
   */
  val vectorNonZeroEndIndex: Int =
    essentialPart.lastIndexWhere(_ != 0) + 2

  /**
   * Apply H on the left of C, in-place
   *
   * i.e. C := H C
   *
   * This needs WorkingArea to be able to provide a vector of size at least
   * as large as the number of columns of C
   *
   * Reference: subroutine DLARF in LAPACK [1] (our implementation
   * differs because we do not have the whole vector of the reflector)
   */
  def applyOnLeft(c:MatrixLike) {
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
      val w = WorkingArea.vector(q)
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
   * This needs WorkingArea to be able to provide a vector of size at least
   * as large as the number of rows of C
   *
   * Reference: subroutine DLARF in LAPACK [1] (our implementation
   * differs because we do not have the whole vector of the reflector)
   */
  def applyOnRight(c:MatrixLike) {
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
      val w = WorkingArea.vector(p)
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
  extends BLAS.level1.Interface
  with EuclideanNorm with NumericPropertiesOfDouble
{
  val safeMin = safeMinimum/epsilonMachine
  val safeMinInv = 1.0/safeMin

  /** Factory method */
  def apply(tau:Double, v:VectorLike): ElementaryReflectorLike

  /** The case $tau = 0$. */
  def identity: ElementaryReflectorLike = this(0, Vector.empty(0))

  /**
   * We follow the conventions and notations of LAPACK [1].
   *
   * Given a vector $y=\C{\alpha}{x}$ of size n, where $\alpha$ is a scalar,
   * and therefore $x$ is a vector of size n-1, we seek the elementary
   * reflector H such that
   * \[
   *      H \C{\alpha}{x} = \C{\beta}{0}
   * \]
   *
   * where $\beta$ is a scalar. The solution is represented as
   * \[
   *     H = I - \tau \C{1}{v} \R{1}{v^T}
   * \]
   *
   * where $\tau$ is a scalar and $v$ is a vector of size n-1 called
   * the essential part of the elementary reflector.
   *
   * If $x=0$, then $\tau=0$ and therefore $H=I$, otherwise $1 <= \tau <= 2$.
   *
   * The construction is done in-place, as vector $y$ get overwritten
   * by $\C{\beta}{v}$
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
    if(y.length <= 1) identity
    else {
      var x = y.block(1, y.length)
      var xNorm = euclideanNorm(x)
      if(xNorm == 0) identity
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
extends ElementaryReflectorLike with BLAS.level1.Naive with BLAS.level2.Naive

object ElementaryReflectorWithNaiveBLAS
extends ElementaryReflectorLikeCompanion
with BLAS.level1.Naive with BLAS.level2.Naive {
  def apply(tau:Double, v:VectorLike) =
    new ElementaryReflectorWithNaiveBLAS(tau, v)
}
