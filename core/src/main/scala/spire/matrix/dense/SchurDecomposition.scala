package spire.matrix.dense.Schur

import spire.matrix.dense._
import spire.matrix.BLAS
import spire.matrix.NumericPropertiesOfDouble
import scala.math._
import java.lang.Math.copySign
import spire.syntax.cfor._

import scala.annotation.tailrec

/**
 * The real Schur decomposition of a Hessenberg matrix.
 *
 * Mathematically, given a Hessenberg matrix H, the real Schur decomposition
 * reads
 * \[
 *   H = Z T Z^T
 * \]
 * where Z is an upper quasi-triangular matrix, i.e. its diagonal is made of
 * either 1 x 1 or 2 x 2 blocks having a pair of complex conjugate eigenvalues,
 * below which all elements are zero.
 *
 * It is assumed that the block H(iLo:iHi, iLo:iHi) is decoupled from the rest
 * of the matrix, i.e. that
 *   - H(iHi:n, iHi:n) is upper quasi-triangular, and that
 *   - H(iLo, iLo-1) = 0 unless iLo = 0.
 * The problem is then simplified to the computation of the Schur decomposition
 * of H(iLo:iHi, iLo:iHi) instead of that of the whole matrix.
 *
 * The input matrix H is not required to have zeroes below the first
 * subdiagonal but the value of those elements is ignored and assumed to be
 * zero. Actually, the algorithm implemented by heirs of this class may zero
 * some subdiagonals below the first one if it simplifies implementation.
 *
 * Moreover the Hessenberg part of H gets overwritten, i.e. the first
 * subdiagonal and the upper triangle:
 *   - if T is requested, then the Hessenberg part of H gets overwritten by it;
 *   - otherwise, the Hessenberg part of H has no useful mathematical meaning
 *     after completion of the algorithm.
 *
 * The algorithm may take as input an orthogonal matrix Q, usually such
 * that $A = Q H Q^T$, where A is the general square matrix whose Schur
 * decomposition is ultimately sought. If provided (i.e. if different from
 * null), Q is transformed in-place into the matrix of Schur vectors $ZQ$.
 * The arguments iLoZ and iHiZ may be used to restrict the computation to the
 * rows of $ZQ$ in the range iLoZ until iHiZ.
 *
 * The argument fullSchurFormWanted specifies whether the full Schur form
 * shall be computed or whether only eigenvalues shall be computed.
 *
 */
class RealDecomposition(val h:Matrix, val q:Option[Matrix],
                        val fullSchurFormWanted: Boolean)
                       (val iLo:Int, val iHi:Int)
                       (val iLoZ:Int, val iHiZ:Int) {

  /** The matrix of Schur vectors Z */
  val z = q

  /** The Schur form T */
  def t = if(fullSchurFormWanted) Some(h.copyToUpperHessenberg) else None

  /** Size of the problem */
  val n = h.dimensions._1

  /**
   * Real and imaginary part of eigenvalues
   *
   * If the i-th and (i+1)-th eigenvalues form a complex conjugate pair, then
   * eigenIm(i) > 0 and eigenIm(i+1) < 0 by convention, and of course
   * eigenIm(i) == 0 if the i-th eigenvalue is real.
   *
   * If the Schur form T is computed, then those eigenvalues are in the
   * same order as the columns (or rows) of T. Specifically,
   *   - if the i-th eigenvalue is real, then eigenRe(i) == T(i,i);
   *   - if the i-th and (i+1)-th eigenvalues are a complex conjugate pair, then
   *     they are the eigenvalues of the block T(i:i+2, i:i+2).
   */
  val eigenvalues = new Array[(Double, Double)](n)
}

trait RealDecompositionCompanion {
  def apply(h:Matrix, q:Option[Matrix]=None,
            fullSchurFormWanted: Boolean=false)
           (iLo:Int=0, iHi:Int=h.dimensions._1)
           (iLoZ:Int=iLo, iHiZ:Int=iHi): RealDecomposition
}


/**
 * The real Schur decomposition of a Hessenberg matrix.
 *
 * It is computed with an implicit single/double-shift QR algorithm.
 *
 * This algorithm has been superseded by multishift algorithms, which do
 * fall back on this algorithms for small submatrices.
 *
 * The implementation factors out many parts into methods which could be
 * overriden (this would make it easy to test different exceptional shifts
 * or different deflation criterion e.g.). The author is not sure whether
 * this is useful and it has to be seen whether this hinders performance.
 *
 * Reference:
 *   - subroutine DLAHQR from [1],
 *   - section 4.4.8 in [3], especially p. 173 on exceptional shifts
 *   - section 7.5 and especially algorithm 7.5.1 in [2]
 *
 * References:
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 *
 *  [2] Gene H. Golub and Charles Van Loan,
 *      Matrix computations,
 *      third ed.,
 *      The John Hopkins University Press, 1996.
 *
 *  [3] James W. Demmel,
 *      Applied numerical linear algebra,
 *      siam, 1997.
 *
 *  [4] R.S. Martin, G. Peters, and J.H. Wilkinson,
 *      The QR algorithm for real hessenberg matrices,
 *      Numerische Mathematik 14 (1970), no. 3, 219–231 (English)
 *
 *  [5] Mario Ahues and Francoise Tisseur,
 *     A new deflation criterion for the QR algorithm,
 *     Tech. Report 122, LAPACK Working Note, March 1997
 */
class RealDecompositionWithDoubleShiftQRAlgorithm(
  h:Matrix, q:Option[Matrix], fullSchurFormWanted: Boolean)
  (iLo:Int, iHi:Int)
  (iLoZ:Int, iHiZ:Int)
extends RealDecomposition(h, q, fullSchurFormWanted)(iLo, iHi)(iLoZ, iHiZ)
with NumericPropertiesOfDouble
with BLAS.level1.Naive
{
  /**
   * Maximum number of iterations to reveal one eigenvalue
   * or one pair of complex conjugate eigenvalues
   */
  var maxIterations = 30

  /**
   * Numeric constants
   */
  val ulp = precision
  val smallNum = safeMinimum*((iHi - iLo).toDouble/ulp)

  /** Constants used in the computation of exceptional shifts */
  val d1: Double =  0.75
  val d2: Double = -0.4375

  // ========== Construction ==========

  if(iLo == iHi-1) {
    eigenvalues(iLo) = (h(iLo, iLo), 0)
  }
  else {
    // Zero the 2nd and 3rd subdiagonal so that further computations, that
    // only touch those and above, see a Hessenberg matrix
    cforRange(iLo to iHi-4) { j =>
      h(j+2, j) = 0
      h(j+3, j) = 0
    }
    if(iLo <= iHi-3) h(iHi-1, iHi-3) = 0

    // Main loop
    // The loop index i decreases from iLhi to iLo by 1 or 2.
    // Each iteration works with H(l:i, l:i) as
    //  - eigenvalues i to iHi-1 have already converged in previous iterations;
    //  - either l=iLo or H(l, l-1) is small enough that the matrix splits.
    var i = iHi
    while(i > iLo) {
      var l = iLo
      var iterations = 0
      var converged = false

      // Perform implicit single/double shift QR iterations on H(iLo:i, iLo:i)
      // until a 1x1 or 2x2 diagonal block splits off at the bottom
      while(!converged) {
        l = findDeflationPoint(l, i)
        if(l > iLo) h(l, l-1) = 0
        if(l >= i-2) converged = true
        else {
           val firstShift = findShifts(l, i, iterations)
           val (m, v) = initialBulge(l, i, firstShift)
           performDoubleShiftQRStep(l, m, i, v)
           iterations += 1
           if(iterations > maxIterations)
            throw new Decomposition.ConvergenceFailure(l, i)
        }
      }

      // Extract eigenvalues of split-off block, 1x1 or 2x2
      if(l == i-1) eigenvalues(i-1) = (h(i-1, i-1), 0)
      else if(l == i-2) perform2x2SchurDecomposition(i)

      // Iterate with new value of i
      i = l
    }
  }

  /**
   * Find k in [l+1, i) such that H(k, k-1) is small enough
   *
   * It uses the conservative deflation criterion introduced in [5]
   */
  final def findDeflationPoint(l:Int, i:Int): Int = {
    // TODO: use cforRange when https://github.com/non/spire/issues/164
    // has been addressed
    for(k <- i-1 until l by -1) {
      // traditional criterion
      if(abs(h(k, k-1)) <= smallNum) return k

      // Ahues & Tisseur criterion
      var tst = abs(h(k-1, k-1)) + abs(h(k, k))
      if(tst == 0) {
        if(k-2 >= iLo) tst += abs(h(k-1, k-2))
        if(k+1 < iHi)  tst += abs(h(k+1, k))
      }
      if(abs(h(k, k-1)) <= ulp*tst) {
        val (h00, h01, h10) = (h(k, k), h(k, k-1), h(k-1, k))
        val h11m00 = h(k-1, k-1) - h00
        val (ab, ba) = if(h01 >= h10) (h01, h10) else (h10, h01)
        val (aa, bb) = if(h00 >= h11m00) (h00, h11m00) else (h11m00, h00)
        val s = aa + ab
        if(ba*(ab/s) <= max(smallNum, ulp*(bb*(aa/s)))) return k
      }
    }
    return l
  }

  // The 2nd shift is complex conjugate of the 1st,
  // which degenerate to their being equal if both are real.
  // This returns the 1st shift as a pair (real part, imaginary part)
  final def findShifts(l:Int, i:Int, iterations:Int): (Double, Double) = {
    // Compute the matrix whose eigenvalues are the shifts
    var (h11, h12,
         h21, h22) = iterations match {
      case 10 => { // exceptional shift to stir convergence
        val s = abs(h(l+1, l)) + abs(h(l+2, l+1))
        val h11 = d1*s + h(l,l)
        (h11, d2*s,
         s  , h11 )
      }
      case 20 => { // exceptional shift to stir convergence
        val s = abs(h(i-1, i-2)) + abs(h(i-2, i-3))
        val h11 = d1*s + h(i-1, i-1)
        (h11, d2*s,
         s  , h11 )
      }
      case _ => { // Francis shift
        (h(i-2, i-2), h(i-2, i-1), // shifts for Francis step
         h(i-1, i-2), h(i-1, i-1))
      }
    }

    // Compute the shifts proper
    val s = abs(h11) + abs(h12) + abs(h21) + abs(h22)
    if(s == 0) (0.0, 0.0)
    else {
      h11 /= s; h12 /= s
      h21 /= s; h22 /= s
      val t = (h11 + h22)/2 // trace/2
      val d = (h11 - t)*(h22 -t) - h12*h21 // = -discriminant/4
      val sd = sqrt(abs(d))
      if(d >= 0) { // complex conjugate shifts
        (t*s, sd*s)
      }
      else { // real shifts (use the closer to h22)
        val s1 = t + sd
        val s2 = t - sd
        val si = s*(if(abs(s1 - h22) <= abs(s2 - h22)) s1 else s2)
        (si, 0.0)
      }
    }
  }

  // Find best index m in [l,i) to perform implicit double-shift QR step
  // on H(m:i, m:i), denoted $A_0$. Returns (m, v) where v is the vector
  // containing the 3 non-zero elements of the first column of the matrix
  // $A_0^2 -2\Re\sigma A_0 +|\sigma|^2 I$ as explained in [3, page 171].
  final
  def initialBulge(l:Int, i:Int, firstShift:(Double, Double)): (Int, Vector) = {
    val v = Vector.empty(3)
    val (sigR, sigI) = firstShift
    // TODO: use cforRange when https://github.com/non/spire/issues/164
    // has been addressed
    for(m <- i-3 to l by -1) {
      val (h11, h12, h21, h22) = (h(m, m), h(m, m+1), h(m+1, m), h(m+1, m+1))
      val h32 = h(m+2, m+1)
      val s = abs(h11 - sigR) + abs(sigI) + abs(h21)
      val h21s = h21/s
      v(0) = h21s*h12 + (h11 - sigR)*((h11 - sigR)/s) + sigI*(sigI/s)
      v(1) = h21s*(h11 + h22 - 2*sigR)
      v(2) = h21s*h32
      val vn = abs(v(0)) + abs(v(1)) + abs(v(2))
      v(0) /= vn; v(1) /= vn; v(2) /= vn
      if(m > l) {
        // Would starting the double-shift QR step at row m result
        // in h(m, m-1) becoming negligible?
        if(abs(h(m, m-1))*(abs(v(1)) + abs(v(2)))
           <=
           ulp*abs(v(0))*(abs(h(m-1, m-1)) + abs(h(m,m)) + abs(h(m+1, m+1))))
          return (m, v)
      }
    }
    (l, v)
  }

  final
  def performDoubleShiftQRStep(l:Int, m:Int, i:Int, v:Vector) {
    // If the Schur form is wanted, the whole matrix H needs to be transformed.
    // Otherwise only the active block l:i.
    val (i1, i2) = if(fullSchurFormWanted) (0, n) else (l, i)
    cforRange(m to i-2) { k =>
      val nr = min(3, i-k) // order of g

      // If this is the 1st iteration, then v is the first column of A_0
      // and we seek the elementary reflector g that produces the first
      // column of its QR reflection (c.f. again [3, page 171]).
      //
      // In subsequent reflection, we are chasing the bulge: we seek an
      // elementary reflector g moving the bulge downward, so we copy the
      // relevant elements of matrix H into v from which we compute g
      if(k > m) copy(h.column(k-1).block(k, k+nr), v)
      val g = TinyElementaryReflector.annihilateAndConstruct(v.block(0, nr))
      if(k > m) {
        // Apply g to column k-1, restoring it to Hessenberg form
        h(k, k-1) = v(0)
        h(k+1, k-1) = 0
        if(k < i-2) h(k+2, k-1) = 0
      }
      else if(m > l) h(m, m-1) *= 1 - g.t0

      g.applyOnLeft(h)(startingRow=k, columns=(k, i2))
      g.applyOnRight(h)(startingColumn=k, rows=(i1, min(k+4, i)))
      q.foreach(g.applyOnRight(_)(startingColumn=k, rows=(iLoZ, iHiZ)))
    }
  }

  final def perform2x2SchurDecomposition(i:Int) {
    val d2x2 = Decomposition2x2(h(i-2 ,i-2), h(i-2, i-1),
                                h(i-1 ,i-2), h(i-1, i-1))
    eigenvalues(i-2) = d2x2.eigenvalues._1
    eigenvalues(i-1) = d2x2.eigenvalues._2
    h.block(i-2, i)(i-2, i) := d2x2.schurForm
    if(fullSchurFormWanted) {
      if(i < n) rot(h.row(i-2).block(i, n),
                    h.row(i-1).block(i, n),
                    d2x2.r)
      rot(h.column(i-2).block(0, i-2),
          h.column(i-1).block(0, i-2),
          d2x2.r)
    }
    // Q -> Q R^T (c.f. definition of d2x2.r)
    q.foreach(q => rot(q.column(i-2).block(iLoZ, iHiZ),
                       q.column(i-1).block(iLoZ, iHiZ),
                       d2x2.r))
  }
}

object RealDecompositionWithDoubleShiftQRAlgorithm
extends RealDecompositionCompanion {

  def apply(h:Matrix, q:Option[Matrix]=None,
            fullSchurFormWanted: Boolean=false)
           (iLo:Int=0, iHi:Int=h.dimensions._1)
           (iLoZ:Int=iLo, iHiZ:Int=iHi) =
    new RealDecompositionWithDoubleShiftQRAlgorithm(
      h, q, fullSchurFormWanted)(iLo, iHi)(iLoZ, iHiZ)
}

/**
 * Schur decomposition of a 2x2 matrix in standard form
 *
 * Mathematically, the Schur decomposition of the 2x2 matrix A reads
 * \[
 *   A = R T R^T
 * \]
 * where R is a rotation matrix and T is
 *   - either upper diagonal, having two real eigenvalues;
 *   - or such that T(0,0) == T(1,1) and T(0,1)*T(1,0) < 0, having two
 *     complex conjugate eigenvalues T(0,0) +/- sqrt(T(0,1)*T(1,0))
 *
 * The matrix T is laid out as
 * <pre>
 *         T = [ a b ]
 *             [ c d ]
 * </pre>
 * whereas the cosine and sine of the rotation are defined as
 * <pre>
 *         R = [ cn -sn ]
 *             [ sn  cn ]
 * </pre>
 *
 * References:
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 *
 */
final class Decomposition2x2(val r: PlaneRotation)
                            (val a:Double, val b:Double,
                             val c:Double, val d:Double)
{
  /**
   * Eigenvalues (eigen1Im >= 0)
   */
  val eigenvalues: ((Double, Double), (Double, Double)) =
    if(c == 0) ((a, 0), (d, 0))
    else {
      val im = sqrt(abs(b))*sqrt(abs(c))
      ((a, im), (d, -im))
    }

  def schurForm = (a, b,
                   c, d)
}

object Decomposition2x2
extends NumericPropertiesOfDouble with EuclideanNorm
{
  val eps = precision

  val tinyThreshold = 4*eps

  /**
   * Schur decomposition of the matrix
   * <pre>
   *         [ a b ]
   *         [ c d ]
   * </pre>
   *
   * References: subroutine DLANV2 from [1]
   *
   * [1] LAPACK Users' Guide.
   *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
   *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
   *     and D Sorensen.
   *     Society for Industrial and Applied Mathematics,
   *     Philadelphia, PA, Third.
   */
  def apply(a:Double, b:Double,
            c:Double, d:Double): Decomposition2x2 = {
    if(c == 0)
      new Decomposition2x2(PlaneRotation.identity)(a, b,
                                                   0, d)

    else if(b == 0) // swap rows and columns
      new Decomposition2x2(PlaneRotation.positiveQuarterRotation)(d, -c,
                                                                  0,  a)
    else if(a - d == 0 && signum(b) != signum(c)) // standard Schur form already
        new Decomposition2x2(PlaneRotation.identity)(a, b,
                                                     c, d)
    else {
      val t = a - d
      val p = 0.5*t
      val bcMax = max(abs(b), abs(c))
      val bcMis = min(abs(b), abs(c))*signum(b)*signum(c)
      val s = max(abs(p), bcMax)
      var z = (p/s)*p + (bcMax/s)*bcMis

      if(z >= tinyThreshold) {
        // Two real eigenvalues, for sure
        z = p + copySign(sqrt(s)*sqrt(z), p)
        val tau = euclideanNorm2D(c, z)
        val r = PlaneRotation(z/tau, c/tau)
        new Decomposition2x2(r)(d + z, b - c,
                                    0, d - (bcMax/z)*bcMis)
      }
      else {
        // Complex eigenvalues, or real almost equal eigenvalues
        val sigma = b + c
        val tau = euclideanNorm2D(sigma, t)
        val cs = sqrt(0.5*(1 + abs(sigma)/tau))
        val sn = -(p/(tau*cs))*signum(sigma)
        val r = PlaneRotation(cs, sn)
        val (aa, bb,
             cc, dd) = r.inverse.applySimilarity((a, b,
                                                  c, d))
        val aORd = 0.5*(aa + dd)
        if(cc == 0)
          new Decomposition2x2(r)(aORd, bb,
                                     0, aORd)
        else if(bb == 0) // compose r with swapping of rows and columns
          new Decomposition2x2(r.timesPositiveQuarterRotation)(aORd, -cc,
                                                                  0,  aORd)
        else if(signum(b) != signum(c)) // standard Schur form
          new Decomposition2x2(r)(aORd, bb,
                                    cc, aORd)
        else { // real eigenvalues: reduce to upper triangular form
          val sab = sqrt(abs(bb))
          val sac = sqrt(abs(cc))
          val p = copySign(sab*sac, cc)
          val tau = 1/sqrt(abs(bb+cc))
          val rr = PlaneRotation(sab*tau, sac*tau)
          new Decomposition2x2(r*rr)(aORd + p, bb - cc,
                                            0, aORd - p)
        }
      }
    }
  }
}


object Decomposition {
  class ConvergenceFailure(start:Int, end:Int)
  extends spire.matrix.dense.Exception(
    s"QR iterations failed to converge for row- and column-range $start:$end")
}
