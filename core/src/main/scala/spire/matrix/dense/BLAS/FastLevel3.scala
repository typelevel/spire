package spire.matrix.dense.BLAS

import spire.syntax.cfor._

import spire.matrix.dense.Matrix
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Sides._
import Transposition._
import UpperOrLower._
import DiagonalProperty._

import scala.math

/**
 * High-performance BLAS level3
 *
 * We closely follow [1,2] but we drew many technical aspects from [3] whose
 * source code proved far more readable than that of GotoBLAS2 while borrowing
 * key ideas from it as well.
 *
 * Given an operation C := α A⨂B + β C (where ⨂ denotes a general matrix
 * multiplication AB, or a triangular solver A^^-1^^ B,
 * or a rank-k update A^^T^^ A, or any of the other BLAS 3 operations),
 * A is decomposed into blocks and B into horizontal panels and the operation
 * is then performed as a series of operations on those submatrices.
 *
 * Reference:
 * [1] Kazushige Goto and Robert A. van de Geijn,
 *     Anatomy of high-performance matrix multiplication,
 *     ACM Transactions on Mathematical Software 34 (2008), no. 3, 12:1–25.
 * [2] Kazushige Goto and Robert van de Geijn,
 *     High-performance implementation of the level-3 BLAS,
 *     Tech. Report FLAME Working Note #20, Technical Report TR-2006-23,
 *     The University of Texas at Austin, Department of Computer Sciences,
 *     May 2006.
 * [3] Gaël Guennebaud, Benoît Jacob, et al.,
 *     Eigen v3, http://eigen.tuxfamily.org, 2010
 */
trait FastLevel3 extends Level3 {
  /**
   * Multiplication of a general block and a general panel
   *
   * Given two matrices A' and B' of respective dimensions m x k and k x n,
   * we are interested in the product C = A B where A is a block of A',
   * A = A'(i1:i1+mc, p1:p1+kc), and B is a horizontal panel of B',
   * B = B'(p1:p1+kc, :). Thus C is the block C'(i1:i1+mc, :) of the product
   * C' = A' B'.
   *
   * This turns out to be the workhorse of high-performance
   * BLAS level3 as explained in details in [1].
   */
  object GEBP {

    type Buffer = FastLevel3Blocking#Buffer

    /**
     * Cut the matrix a in slices of nr columns, with a row-major arrangement
     *
     * As an illustration, for `nr = 4`, and a 10 x 5 matrix, the layout is:
     * <pre>
     *     0  1  2  3   20 21 22 23   40 41
     *     4  5  6  7   24 25 26 27   42 43
     *     8  9 10 11   28 29 30 31   44 45
     *    12 13 14 15   32 33 34 35   46 47
     *    16 17 18 19   36 37 38 39   48 49
     * </pre>
     *
     * Requirement: nr == 2 or 4
     */
    def packColumnSlices(a:Matrix, nr:Int, aa:Buffer) {
      val (m,n) = a.dimensions
      val nn = (n/nr)*nr
      var s = 0
      val ld = a.ld
      val o = a.start
      val e = a.elements
      //cforRange(0 until nn by nr) { j =>
      cfor(0)(_ < nn, _ + nr) { j =>
        // start of column j, j+1, j+2 and j+4
        val r0 = o + (j+0)*ld
        val r1 = o + (j+1)*ld
        val r2 = o + (j+2)*ld
        val r3 = o + (j+3)*ld
        cforRange(0 until m) { i =>
                      aa(s+0) = e(r0+i)
                      aa(s+1) = e(r1+i)
          if(nr == 4) aa(s+2) = e(r2+i)
          if(nr == 4) aa(s+3) = e(r3+i)
          s += nr
        }
      }
      cforRange(nn until n) { j =>
        val r0 = o + (j+0)*ld
        cforRange(0 until m) { i =>
          aa(s) = e(r0+i)
          s += 1
        }
      }
    }

    /**
     * Cut the matrix a in slices of mr rows with a column-major arrangement
     *
     * As an illustration, for `mr = 4` and a a 7 x 5 matrix, the layout is:
     * <pre>
     *      0   4  8 12 16
     *      1   5  9 13 17
     *      2   6 10 14 18
     *      3   7 11 15 19
     *
     *      20 23 26 29 32
     *      21 24 27 30 33
     *      22 25 28 31 34
     *
     * Requirement: mr == 2 or 4
     * </pre>
     */
    def packRowSlices(a:Matrix, mr:Int, aa:Buffer) {
      val (m,n) = a.dimensions
      val mm = (m/mr)*mr
      var s = 0
      val ld = a.ld
      val e = a.elements
      //cforRange(0 until mm by mr) { i =>
      cfor(0)(_ < mm, _ + mr) { i =>
        cforRange(0 until n) { j =>
          val r = i + j*ld
                      aa(s+0) = e(r+0)
                      aa(s+1) = e(r+1)
          if(mr == 4) aa(s+2) = e(r+2)
          if(mr == 4) aa(s+3) = e(r+3)
          s += mr
        }
      }
      cforRange(mm until m) { i =>
        cforRange(0 until n) { j =>
          val r = i + j*ld
          aa(s+0) = e(r+0)
          s += 1
        }
      }
    }

    /**
     * Perform the actual block-panel product
     *
     * This computes C(0:mc, 0:n) = A(0:mc, 0:kc) B(0:kc, 0:n)
     * A(0:mc, 0:kc) shall have been stored in aa by packA
     * and B(0:kc, 0:n) shall have been stored in bb by packB.
     * The result is accumulated in C'(i1:i1+mc, 0:n):
     *   C'(i1:i1+mc, 0:n) = α C'(i1:i1+mc, 0:n) + β C(0:mc, 0:n)
     *
     * Implementation notes:
     * the optimal value of mr on most modern processors according to [1]
     * is mr = 4 (for double precision) but the kernel explicitly uses vector
     * register holding 2 doubles, in effect applying the algorithm
     * implemented here for mr = 2. We can't do that in Java, so we settle
     * for mr = 2 with scalars for the time being. Furthermore, hardcoded
     * for nr = 4 as well at the moment (because this is the optimal value on
     * most modern processors).
     *
     * This code is adapted from Eigen gebp_kernel in
     * core/products/GeneralBlockPanelKernel.h
     */
    def apply(kc:Int, mr:Int, nr:Int,
              alpha:Double, aa:Buffer, bb:Buffer, beta:Double, c:Matrix)
    {
      val (mc, n) = c.dimensions
      val m2 = (mc/2)*2
      val k4 = (kc/4)*4
      val n4 = (n/4)*4

      val cc = c.elements
      val ldC = c.ld
      val startC = c.start

      // Compute C(:, 0:n4) = A(:, :) B(:, 0:n4)
      cforRange(0 until n4 by 4) { j =>
        // Compute C(0:m2, j:j+4) = A(0:m2, :) B(:, j:j+4)
        cforRange(0 until m2 by 2) { i =>
          // Compute C(i:i+2, j:j+4) += A(i:i+2, :) B(:, j:j+4)

          // Indexing into cc
          val r0 = startC + i + j*ldC
          val r1 = r0 + ldC
          val r2 = r1 + ldC
          val r3 = r2 + ldC

          // Store in registers (hopefully!):
          //   C(i:i+2, j:j+4) = [ c0 c1 c2 c3 ]
          //                     [ c4 c5 c6 c7 ]
          var c0, c1, c2, c3, c4, c5, c6, c7 = 0.0

          // Compute C(i:i+2, j:j+4) += A(i:i+2, 0:k4) B(0:k4, j:j+4)
          // as a sum of [2 x 4] [4 x 4] matrix products
          var paa = i*kc
          var pbb = j*kc
          cforRange(0 until k4 by 4) { p =>
            // Compute C(i:i+2, j:j+4) += A(i:i+2, p:p+4) B(p:p+4, j:j+4)
            // --------\/-------------    -----\/-------  -------\/-----
            //         cw                      aw                bw
            //
            // this is the unrolled version of:
            // for k = 1..4
            //   load aw(:,k)
            //   for j = 1..4
            //     cw(:,j) += aw(:,k) b(k,j)
            //
            // Thus aw and bw are expected to be packed column- and row-major
            // respectively (the packing of aa and bb prior to calling this
            // function should have laid out elements in that manner).
            var a0 = aa(paa+0)
            var a1 = aa(paa+1)
            var b0 = bb(pbb+0)
            var b1 = bb(pbb+1)
            c0 += a0*b0
            var b2 = bb(pbb+2)
            c4 += a1*b0
            var b3 = bb(pbb+3)
            b0 = bb(pbb+4)
            c1 += a0*b1
            c5 += a1*b1
            b1 = bb(pbb+5)
            c2 += a0*b2
            c6 += a1*b2
            b2 = bb(pbb+6)
            c3 += a0*b3
            a0 = aa(paa+2)
            c7 += a1*b3
            a1 = aa(paa+3)
            b3 = bb(pbb+7)
            c0 += a0*b0
            c4 += a1*b0
            b0 = bb(pbb+8)
            c1 += a0*b1
            c5 += a1*b1
            b1 = bb(pbb+9)
            c2 += a0*b2
            c6 += a1*b2
            b2 = bb(pbb+10)
            c3 += a0*b3
            a0 = aa(paa+4)
            c7 += a1*b3
            a1 = aa(paa+5)
            b3 = bb(pbb+11)
            c0 += a0*b0
            c4 += a1*b0
            b0 = bb(pbb+12)
            c1 += a0*b1
            c5 += a1*b1
            b1 = bb(pbb+13)
            c2 += a0*b2
            c6 += a1*b2
            b2 = bb(pbb+14)
            c3 += a0*b3
            a0 = aa(paa+6)
            c7 += a1*b3
            a1 = aa(paa+7)
            b3 = bb(pbb+15)
            c0 += a0*b0
            c4 += a1*b0
            c1 += a0*b1
            c5 += a1*b1
            c2 += a0*b2
            c6 += a1*b2
            c3 += a0*b3
            c7 += a1*b3

            // next pair of blocks
            paa += 8
            pbb += 16
          }

          // Compute C(i:i+2, j:j+4) += A(i:i+2, k4:kc) B(k4:kc, j:j+4)
          // one index p at a time
          cforRange(k4 until kc) { p =>
            val a0 = aa(paa)
            val a1 = aa(paa+1)
            val b0 = bb(pbb)
            val b1 = bb(pbb+1)
            c0 += a0*b0
            val b2 = bb(pbb+2)
            c4 += a1*b0
            val b3 = bb(pbb+3)
            c1 += a0*b1
            c5 += a1*b1
            c2 += a0*b2
            c6 += a1*b2
            c3 += a0*b3
            c7 += a1*b3

            // next line
            paa += 2
            pbb += 4
          }

          // Store result:
          //   C(i:i+2, j:j+4) = α P'(i:i+2, j:j+4) + 𝜷 C(i:i+2, j:j+4)
          c0 *= alpha; c1 *= alpha; c2 *= alpha; c3 *= alpha
          c4 *= alpha; c5 *= alpha; c6 *= alpha; c7 *= alpha
          if(beta == 1) {
            cc(r0)   += c0
            cc(r1)   += c1
            cc(r2)   += c2
            cc(r3)   += c3
            cc(r0+1) += c4
            cc(r1+1) += c5
            cc(r2+1) += c6
            cc(r3+1) += c7
          }
          else {
            cc(r0)   = beta*cc(r0)   + c0
            cc(r1)   = beta*cc(r1)   + c1
            cc(r2)   = beta*cc(r2)   + c2
            cc(r3)   = beta*cc(r3)   + c3
            cc(r0+1) = beta*cc(r0+1) + c4
            cc(r1+1) = beta*cc(r1+1) + c5
            cc(r2+1) = beta*cc(r2+1) + c6
            cc(r3+1) = beta*cc(r3+1) + c7
          }
        }

        // Compute C(m2, j:j+4) += A(m2, :) B(:, j:j+4) if need be
        if(m2 != mc) {

          // Store in registers (hopefully!):
          //   C(m2, j:j+4) = [ c0 c1 c2 c3 ]
          var c0, c1, c2, c3 = 0.0

          // Indexing into cc
          val r0 = startC + m2 + j*ldC
          val r1 = r0 + ldC
          val r2 = r1 + ldC
          val r3 = r2 + ldC

          // Compute C(m2, j:j+4) += A(m2, 0:k4) B(0:k4, j:j+4)
          var paa = m2*kc
          var pbb = j*kc
          cforRange(0 until k4 by 4) { p =>
            var a0 = aa(paa+0)
            var b0 = bb(pbb+0)
            var b1 = bb(pbb+1)
            c0 += a0*b0
            var b2 = bb(pbb+2)
            var b3 = bb(pbb+3)
            b0 = bb(pbb+4)
            c1 += a0*b1
            b1 = bb(pbb+5)
            c2 += a0*b2
            b2 = bb(pbb+6)
            c3 += a0*b3
            a0 = aa(paa+1)
            b3 = bb(pbb+7)
            c0 += a0*b0
            b0 = bb(pbb+8)
            c1 += a0*b1
            b1 = bb(pbb+9)
            c2 += a0*b2
            b2 = bb(pbb+10)
            c3 += a0*b3
            a0 = aa(paa+2)
            b3 = bb(pbb+11)
            c0 += a0*b0
            b0 = bb(pbb+12)
            c1 += a0*b1
            b1 = bb(pbb+13)
            c2 += a0*b2
            b2 = bb(pbb+14)
            c3 += a0*b3
            a0 = aa(paa+3)
            b3 = bb(pbb+15)
            c0 += a0*b0
            c1 += a0*b1
            c2 += a0*b2
            c3 += a0*b3

            // next pair of blocks
            paa += 4
            pbb += 16
          }

          // Compute C(m2, j:j+4) += A(m2, k4:k) B(k4:k, j:j+4)
          cforRange(k4 until kc) { p =>
            val a0 = aa(paa)
            val b0 = bb(pbb+0)
            val b1 = bb(pbb+1)
            val b2 = bb(pbb+2)
            val b3 = bb(pbb+3)
            c0 += a0*b0
            c1 += a0*b1
            c2 += a0*b2
            c3 += a0*b3

            // next line
            paa += 1
            pbb += 4
          }

          // Store result:
          //   C(m2, j:j+4) = α C(m2, j:j+4) + 𝜷 C(m2, j:j+4)
          c0 *= alpha; c1 *= alpha; c2 *= alpha; c3 *= alpha
          if(beta == 1) {
            cc(r0)   += c0
            cc(r1)   += c1
            cc(r2)   += c2
            cc(r3)   += c3
          }
          else {
            cc(r0)   = beta*cc(r0)   + c0
            cc(r1)   = beta*cc(r1)   + c1
            cc(r2)   = beta*cc(r2)   + c2
            cc(r3)   = beta*cc(r3)   + c3
          }
        }
      }

      // Compute A(0:m2, :) B(:, n4:n)
      cforRange(n4 until n) { j =>
        // compute A(0:m2, :) B(:, j)
        cforRange(0 until m2 by 2) { i =>
          // Compute C(i:i+2, j) += A(i:i+2, :) B(:, j)
          // using a straightforward summation

          // Indexing into cc
          val r0 = startC + i + j*ldC

          // Store in registers (hopefully!):
          //   C(i:i+2, j) = [ c0 ]
          //                 [ c4 ]
          var c0, c4 = 0.0
          var paa = i*kc
          var pbb = j*kc
          cforRange(0 until kc) { p =>
            val a0 = aa(paa)
            val a1 = aa(paa+1)
            val b0 = bb(pbb)
            c0 += a0*b0
            c4 += a1*b0
            pbb += 1
            paa += 2
          }

          // Store results in C
          c0 *= alpha; c4 *= alpha
          if(beta == 1) {
            cc(r0)   += c0
            cc(r0+1) += c4
          }
          else {
            cc(r0)   = beta*cc(r0)   + c0
            cc(r0+1) = beta*cc(r0+1) + c4
          }
        }

        // Compute C(m2, j) += A(m2, :) B(:, j) if need be
        if(m2 != mc) {
          var c0 = 0.0
          var r0 = startC + m2 + j*ldC
          var paa = m2*kc
          var pbb = j*kc
          cforRange(0 until kc) { p =>
            val a0 = aa(paa)
            val b0 = bb(pbb)
            c0 += a0*b0
            pbb += 1
            paa += 1
          }
          c0 *= alpha
          if(beta == 1) cc(r0) += c0 else cc(r0) = beta*cc(r0) + c0
        }
      }
    }
  }

  def gemm(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:Matrix, b:Matrix,
           beta:Double, c:Matrix) {
    checkGemmPreconditions(transA, transB, alpha, a, b, beta, c)

    // trivial cases
    val (m, n) = c.dimensions
    if(alpha == 0) {
      if(beta == 0)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) = 0 }
      else if(beta != 1)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) *= beta }
      return
    }

    // Charge!
    val k = if(transB == NoTranspose) b.dimensions._1 else b.dimensions._2
    val blocking = FastLevel3Blocking()
    val mc = blocking.mc
    val kc = blocking.kc
    val mr = blocking.mr
    val nr = blocking.nr

    val aa = blocking.bufferA
    val bb = blocking.bufferB(n)

    val panelB = if(transB == NoTranspose) (pLo:Int,pHi:Int) =>
                   b.block(pLo,pHi)(0,n)
                 else                      (pLo:Int,pHi:Int) =>
                   b.block(0,n)(pLo,pHi)
    val blockA = if(transA == NoTranspose) (iLo:Int,iHi:Int, pLo:Int,pHi:Int) =>
                   a.block(iLo,iHi)(pLo,pHi)
                 else                      (iLo:Int,iHi:Int, pLo:Int,pHi:Int) =>
                   a.block(pLo,pHi)(iLo,iHi)

    val packB = if(transB == NoTranspose) GEBP.packColumnSlices _
                else                      GEBP.packRowSlices _
    val packA = if(transA == NoTranspose) GEBP.packRowSlices _
                else                      GEBP.packColumnSlices _

    cfor(0)(_ < k, _ + kc) { p =>
      // the last panel is likely smaller
      val kc1 = math.min(p + kc, k) - p

      // we are going to compute the following panel-panel product:
      //   A(:, p:p+kc1) B(p:p+kc1, :)
      //   ------\/----- -----\/------
      //     vertical      horizontal
      //     panel         panel

      // pack B(p:p+kc1, :) in a buffer
      packB(panelB(p,p+kc1), nr, bb)

      cfor(0)(_ < m, _ + mc) { i =>
        // the last panel is likely smaller
        val mc1 = math.min(i + mc, m) -i

        // we are going to compute the following block-panel product:
        //   A(i:i+mc1, p:p+kc1) B(p:p+kc1, :)

        // pack A(i:i+mc1, p:p+kc1) in a buffer
        packA(blockA(i,i+mc1, p,p+kc1), mr, aa)

        // block x panel
        GEBP(kc1, mr, nr, alpha, aa, bb, beta, c.block(i,i+mc1)(0,n))
      }
    }
  }

  def syrk(uplo:UpperOrLower.Value, trans:Transposition.Value,
           alpha:Double, a:Matrix, beta:Double, c:Matrix) {
    throw new NotImplementedError("Fast Symmetric Rank-K update")
  }

  def trsm(side:Sides.Value, uplo:UpperOrLower.Value,
           trans:Transposition.Value, unit:DiagonalProperty.Value,
           alpha:Double, a:Matrix, B:Matrix) {
    throw new NotImplementedError("Fast TRiangular Solver with Multiple rhs")
  }
}

/** Convenience object to enable `import` instead of `extends` */
object FastLevel3 extends FastLevel3


class FastLevel3Blocking(val mc:Int, val kc:Int,
                         val mr:Int, val nr:Int,
                         val np:Int) {
  type Buffer = Array[Double]

  /** Buffer to store a block of A of size mc x kc */
  val bufferA = new Buffer(mc*kc)

  private var actualBufferB = new Buffer(kc*np)
  private var actualBufferBCols = np

  /** Buffer to store a panel of B of size kc x n */
  def bufferB(n:Int) = {
    if(n >= actualBufferBCols) {
      actualBufferB = new Buffer(kc*n)
      actualBufferBCols = n
    }
    actualBufferB
  }
}

object FastLevel3Blocking {
  private val tl = new ThreadLocal[FastLevel3Blocking] {
    override def initialValue =
      new FastLevel3Blocking(mc=512, kc=256, mr=2, nr=4, np=512)
  }

  def apply() = tl.get()
}



