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
trait LayeredLevel3 extends Level3 {

  trait Buffer {
    import sun.misc.Unsafe
    // Using Unsafe.getUnsafe restrics users to trusted code,
    // hence this hack!
    private val unsafe = {
      val fld = classOf[Unsafe].getDeclaredField("theUnsafe")
      fld.setAccessible(true)
      fld.get(classOf[Unsafe]).asInstanceOf[Unsafe]
    }

    final def get(p:Long, i:Int) = unsafe.getDouble(p + i*8)

    final def put(p:Long, i:Int, v:Double) { unsafe.putDouble(p + i*8, v) }

    def display(title:String, p:Long, size:Int) {
      print(s"$title = {")
      cforRange(0 until size) { i => print(GEBP.get(p, i)); print(", ") }
      println("}")
    }

    def getArray(p:Long, size:Int) = {
      val result = new Array[Double](size)
      cforRange(0 until size) { i => result(i) = get(p, i) }
      result
    }

    def newBuffer(size:Int) = unsafe.allocateMemory(size*8)

    def resizeBuffer(start:Long,
                     size:Int) = unsafe.reallocateMemory(start, size*8)

    def freeBuffer(start:Long) { unsafe.freeMemory(start) }
  }

  /**
   * Multiplication of a general block and a general panel
   *
   * Given two matrices A' and B' of respective dimensions m x k and k x n,
   * we are interested in the product C = A B where A is a block of A',
   * A = A'(i:i+mc, p:p+kc), and B is a horizontal panel of B',
   * B = B'(p:p+kc, :). Thus C is a contribution to the block C'(i:i+mc, :)
   * of the product C' = A' B'.
   *
   * This turns out to be the workhorse of high-performance
   * BLAS level3 as explained in details in [1].
   */
  object GEBP extends Buffer {

    /** The blocking policy and buffers used by GEBP */
    class Blocking(val mc:Int, val kc:Int,
                   val mr:Int, val nr:Int,
                   val np:Int) {

      /** Buffer to store a block of A of size mc x kc */
      val bufferA = newBuffer(mc*kc)

      private var actualBufferB = newBuffer(kc*np)
      private var actualBufferBCols = np

      /** Buffer to store a panel of B of size kc x n */
      def bufferB(n:Int) = {
        if(n >= actualBufferBCols) {
          actualBufferB = resizeBuffer(actualBufferB, kc*n)
          actualBufferBCols = n
        }
        actualBufferB
      }

      override def finalize() {
        freeBuffer(bufferA)
        freeBuffer(actualBufferB)
      }
    }

    /** Provides per-thread instance of blocking */
    val threadLocalBlocking = new ThreadLocal[Blocking] {
        override def initialValue =
          new Blocking(mc=512, kc=256, mr=2, nr=4, np=512)
    }

    /**
     * Cut the matrix b in slices of nr columns, with a row-major arrangement
     *
     * As an illustration, for `nr = 4`, and a 10 x 5 matrix, the layout is:
     * <pre>
     *     0  1  2  3   20 21 22 23   40 45
     *     4  5  6  7   24 25 26 27   41 46
     *     8  9 10 11   28 29 30 31   42 47
     *    12 13 14 15   32 33 34 35   43 48
     *    16 17 18 19   36 37 38 39   44 49
     * </pre>
     *
     * This packed layout is stored in the buffer starting at address bb
     *
     * Requirement: nr == 2 or 4. Not enforced for performance reason,
     * i.e. using any other value will lead to disaster.
     */
    def packColumnSlices(reverseRows:Boolean)(b:Matrix, nr:Int, bb:Long) {
      val (kc,n) = b.dimensions
      val nn = (n/nr)*nr
      var pbb = bb
      val ld = b.ld
      val o = b.start
      val e = b.elements
      val (rowStart, rowInc, rowEnd) =
        if(reverseRows) (kc-1, -1, -1)
        else            (0   , +1, kc)
      cfor(0)(_ < nn, _ + nr) { j =>
        // start of column j, j+1, j+2 and j+3
        val r0 = o + (j+0)*ld
        val r1 = o + (j+1)*ld
        val r2 = o + (j+2)*ld
        val r3 = o + (j+3)*ld
        cfor(rowStart)(_ != rowEnd, _ + rowInc) { p =>
                      put(pbb, 0, e(r0+p))
                      put(pbb, 1, e(r1+p))
          if(nr == 4) put(pbb, 2, e(r2+p))
          if(nr == 4) put(pbb, 3, e(r3+p))
          pbb += nr * 8
        }
      }
      cforRange(nn until n) { j =>
        val r0 = o + (j+0)*ld
        cfor(rowStart)(_ != rowEnd, _ + rowInc) { p =>
          put(pbb, 0, e(r0+p))
          pbb += 1 * 8
        }
      }
    }

    /**
     * The inverse operation of packColumnSlices
     *
     * @parameter alpha: elements from the buffer bb are multiplied by it
     *                   before being copied back to b
     */
    def unpackColumnSlices(reverseRows:Boolean)
                          (alpha:Double, bb:Long, nr:Int, b:Matrix) {
      val (kc,n) = b.dimensions
      val nn = (n/nr)*nr
      var pbb = bb
      val ld = b.ld
      val o = b.start
      val e = b.elements
      val (rowStart, rowInc, rowEnd) =
        if(reverseRows) (kc-1, -1, -1)
        else            (0   , +1, kc)
      cfor(0)(_ < nn, _ + nr) { j =>
        // start of column j, j+1, j+2 and j+3
        val r0 = o + (j+0)*ld
        val r1 = o + (j+1)*ld
        val r2 = o + (j+2)*ld
        val r3 = o + (j+3)*ld
        cfor(rowStart)(_ != rowEnd, _ + rowInc) { p =>
                      e(r0+p) = alpha*get(pbb, 0)
                      e(r1+p) = alpha*get(pbb, 1)
          if(nr == 4) e(r2+p) = alpha*get(pbb, 2)
          if(nr == 4) e(r3+p) = alpha*get(pbb, 3)
          pbb += nr * 8
        }
      }
      cforRange(nn until n) { j =>
        val r0 = o + (j+0)*ld
        cfor(rowStart)(_ != rowEnd, _ + rowInc) { p =>
          e(r0+p) = alpha*get(pbb, 0)
          pbb += 1 * 8
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
     *      20 21 22 23 24
     *      25 26 27 28 29
     *      30 31 32 33 34
     * </pre>
     *
     * This packed layout is stored in the buffer starting at address bb
     *
     * Requirement: mr == 2 or 4. Not enforced for performance reason,
     * i.e. using any other value will lead to disaster.
     */
    def packRowSlices(reverseColumns:Boolean)(a:Matrix, mr:Int, aa:Long) {
      val (mc,nc) = a.dimensions
      val mmc = (mc/mr)*mr
      var paa = aa
      val ld = a.ld
      val o = a.start
      val e = a.elements
      val (colStart, colInc, colEnd) =
        if(reverseColumns) (nc-1, -1, -1)
        else               (   0, +1, nc)
      cfor(0)(_ < mmc, _ + mr) { i =>
        cfor(colStart)(_ != colEnd, _ + colInc) { j =>
          val r = o + i + j*ld
                      put(paa, 0, e(r+0))
                      put(paa, 1, e(r+1))
          if(mr == 4) put(paa, 2, e(r+2))
          if(mr == 4) put(paa, 3, e(r+3))
          paa += mr * 8
        }
      }
      cforRange(mmc until mc) { i =>
        cfor(colStart)(_ != colEnd, _ + colInc) { j =>
          put(paa, 0, a(i,j))
          paa += 1 * 8
        }
      }
    }

    /**
     * The inverse operation of packRowSlices
     *
     * @parameter alpha: elements from the buffer aa are multiplied by it
     *                   before being copied back to a
     */
    def unpackRowSlices(reverseColumns:Boolean)
                       (alpha:Double, aa:Long, mr:Int, a:Matrix) {
      val (mc,nc) = a.dimensions
      val mmc = (mc/mr)*mr
      var paa = aa
      val ld = a.ld
      val o = a.start
      val e = a.elements
      val (colStart, colInc, colEnd) =
        if(reverseColumns) (nc-1, -1, -1)
        else               (   0, +1, nc)
      cfor(0)(_ < mmc, _ + mr) { i =>
        cfor(colStart)(_ != colEnd, _ + colInc) { j =>
          val r = o + i + j*ld
                      e(r+0) = alpha*get(paa, 0)
                      e(r+1) = alpha*get(paa, 1)
          if(mr == 4) e(r+2) = alpha*get(paa, 2)
          if(mr == 4) e(r+3) = alpha*get(paa, 3)
          paa += mr * 8
        }
      }
      cforRange(mmc until mc) { i =>
        cfor(colStart)(_ != colEnd, _ + colInc) { j =>
          a(i,j) = alpha*get(paa, 0)
          paa += 1 * 8
        }
      }
    }

    /**
     * Perform the actual block-panel product
     *
     * This computes C += A B where C is mc x n, A is mc x kc and B is kc x n.
     * A (resp. B) shall have been stored in a buffer starting at address aa (
     * resp. bb) by packA (resp. packB).
     *
     * Implementation notes:
     *   1. the optimal value of mr on most modern processors according to [1]
     *   is mr = 4 (for double precision) but the kernel explicitly uses vector
     *   register holding 2 doubles, in effect applying the algorithm
     *   implemented here for mr = 2. We can't do that in Java, so we settle
     *   for mr = 2 with scalars for the time being. Furthermore, hardcoded
     *   for nr = 4 as well at the moment (because this is the optimal value on
     *   most modern processors).
     *
     *   2. Using sun.misc.Unsafe for the buffers that store the block A and
     *   the panel B, as opposed to using Array[Double], turns out to be
     *   essential here. A careful comparison of the Intel assembly emitted
     *   by Hotspot 64-bit 23.7-b01 (shipped with JRE 1.7.0_17-b02) for the
     *   hottest loop when using Array[Double] shows that many daload's result
     *   in a pair of instructions like:
     *         mov    r11,QWORD PTR [rsp+0xa0]
     *         movsd  xmm13,QWORD PTR [r11+r8*8+0x10]
     *   Thus an address is repeatedly fetched from the stack (mov), which is
     *   then used to load the double precision float (movsd).
     *   On the contrary, with Unsafe, there is only a movsd.
     *
     *   This has 2 adverse effects:
     *     a. since the efficiency of GEBP hinges on those buffers filling as
     *     much of the machine caches (TLB, L2 and L1) and staying there for
     *     the whole execution of this method `apply`. The repeated fetching
     *     from the stack dramatically kills performance;
     *     b. increase the instruction count by about 1/3 and more importantly
     *     clog up the pipeline with those useless mov's
     *
     * Requirement: mr == 2 and nr == 4. Not enforced for performance reason,
     * i.e. using any other value will lead to disaster.
     */
    def apply(kc:Int, mr:Int, nr:Int, alpha:Double, aa:Long, bb:Long, c:Matrix)
    {
      val (mc, n) = c.dimensions
      val m2 = (mc/2)*2
      val n4 = (n/4)*4

      val cc = c.elements
      val ldC = c.ld
      val startC = c.start

      // Compute C(:, 0:n4) += A(:, :) B(:, 0:n4)
      cforRange(0 until n4 by 4) { j =>
        // Compute C(0:m2, j:j+4) += A(0:m2, :) B(:, j:j+4)
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

          // This is the hottest loop.
          //
          // Implementation from [1] and [3] unroll the loops by hand
          // and carefully interspred the get's with the arithmetic operations
          // so as to reduce dependencies between instructions,which makes the
          // code more efficient on the superscalar pipelined processors of
          // this era. But the JIT does not respect this ordering and tries
          // to figure it out on its own, with little success because the
          // unrolled code ends up being too complex. For example,
          // c0, c1, ..., c7 end up being stored on the stack instead of being
          // held in registers, with disastrous consequences for performance.
          //
          // Hence keep it simple to help the JIT makes the right decisions.
          var paa = aa + i*kc * 8
          var pbb = bb + j*kc * 8
          cforRange(0 until kc) { p =>
            val a0 = get(paa, 0)
            val a1 = get(paa, 1)
            val b0 = get(pbb, 0)
            val b1 = get(pbb, 1)
            val b2 = get(pbb, 2)
            val b3 = get(pbb, 3)
            c0 += a0*b0
            c1 += a0*b1
            c2 += a0*b2
            c3 += a0*b3
            c4 += a1*b0
            c5 += a1*b1
            c6 += a1*b2
            c7 += a1*b3

            // next line
            paa += 2 * 8
            pbb += 4 * 8
          }

          // Store result:
          //   C(i:i+2, j:j+4) += α P'(i:i+2, j:j+4)
          c0 *= alpha; c1 *= alpha; c2 *= alpha; c3 *= alpha
          c4 *= alpha; c5 *= alpha; c6 *= alpha; c7 *= alpha
          cc(r0)   += c0
          cc(r1)   += c1
          cc(r2)   += c2
          cc(r3)   += c3
          cc(r0+1) += c4
          cc(r1+1) += c5
          cc(r2+1) += c6
          cc(r3+1) += c7
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

          var paa = aa + m2*kc * 8
          var pbb = bb + j*kc  * 8
          cforRange(0 until kc) { p =>
            val a0 = get(paa, 0)
            val b0 = get(pbb, 0)
            val b1 = get(pbb, 1)
            val b2 = get(pbb, 2)
            val b3 = get(pbb, 3)
            c0 += a0*b0
            c1 += a0*b1
            c2 += a0*b2
            c3 += a0*b3

            // next line
            paa += 1 * 8
            pbb += 4 * 8
          }

          // Store result:
          //   C(m2, j:j+4) = α C(m2, j:j+4) + 𝜷 C(m2, j:j+4)
          c0 *= alpha; c1 *= alpha; c2 *= alpha; c3 *= alpha
          cc(r0)   += c0
          cc(r1)   += c1
          cc(r2)   += c2
          cc(r3)   += c3
        }
      }

      // Compute C(:, n4:n) += A(:, :) B(:, n4:n)
      cforRange(n4 until n) { j =>
        // compute C(0:m2, j) += A(0:m2, :) B(:, j)
        cforRange(0 until m2 by 2) { i =>
          // Compute C(i:i+2, j) += A(i:i+2, :) B(:, j)
          // using a straightforward summation

          // Indexing into cc
          val r0 = startC + i + j*ldC

          // Store in registers (hopefully!):
          //   C(i:i+2, j) = [ c0 ]
          //                 [ c4 ]
          var c0, c4 = 0.0
          var paa = aa + i*kc * 8
          var pbb = bb + j*kc * 8
          cforRange(0 until kc) { p =>
            val a0 = get(paa, 0)
            val a1 = get(paa, 1)
            val b0 = get(pbb, 0)
            c0 += a0*b0
            c4 += a1*b0
            pbb += 1 * 8
            paa += 2 * 8
          }

          // Store results in C
          c0 *= alpha; c4 *= alpha
          cc(r0)   += c0
          cc(r0+1) += c4
        }

        // Compute C(m2, j) += A(m2, :) B(:, j) if need be
        if(m2 != mc) {
          var c0 = 0.0
          var r0 = startC + m2 + j*ldC
          var paa = aa + m2*kc * 8
          var pbb = bb + j*kc  * 8
          cforRange(0 until kc) { p =>
            val a0 = get(paa, 0)
            val b0 = get(pbb, 0)
            c0 += a0*b0
            pbb += 1 * 8
            paa += 1 * 8
          }
          c0 *= alpha
          cc(r0) += c0
        }
      }
    }
  }

  def gemm(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:Matrix, b:Matrix,
           beta:Double, c:Matrix)
  {
    checkGemmPreconditions(transA, transB, alpha, a, b, beta, c)

    val (m, n) = c.dimensions
    val k = if(transB == NoTranspose) b.dimensions._1 else b.dimensions._2

    val blocking = GEBP.threadLocalBlocking.get()
    val mc = blocking.mc
    val kc = blocking.kc
    val mr = blocking.mr
    val nr = blocking.nr

    // trivial case
    if(alpha == 0) {
      trivialGemm(transA, transB, a, b, beta, c)
      return
    }

    // Charge!
    val aa = blocking.bufferA
    val bb = blocking.bufferB(n)

    val panelB =
      if(transB == NoTranspose)
        (pLo:Int,pHi:Int) => b.block(pLo,pHi)(0,n)
      else
        (pLo:Int,pHi:Int) => b.block(0,n)(pLo,pHi)

    val blockA =
      if(transA == NoTranspose)
        (iLo:Int,iHi:Int, pLo:Int,pHi:Int) => a.block(iLo,iHi)(pLo,pHi)
      else
        (iLo:Int,iHi:Int, pLo:Int,pHi:Int) => a.block(pLo,pHi)(iLo,iHi)

    val packB = if(transB == NoTranspose) GEBP.packColumnSlices(false) _
                else                      GEBP.packRowSlices(false) _
    val packA = if(transA == NoTranspose) GEBP.packRowSlices(false) _
                else                      GEBP.packColumnSlices(false) _

    if(beta == 0)
      c := 0
    else if(beta != 1)
      cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) *= beta }

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
        GEBP(kc1, mr, nr, alpha, aa, bb, c.block(i,i+mc1)(0,n))
      }
    }
  }

  def syrk(uplo:UpperOrLower.Value, trans:Transposition.Value,
           alpha:Double, a:Matrix, beta:Double, c:Matrix) {
    throw new NotImplementedError("Fast Symmetric Rank-K update")
  }

  /**
   * Triangular solver for a block and a panel of a larger problem
   *
   * Mathematically, solving for X either A' X = B or X A' = B
   * where A' = A or A^T and where A is either lower or upper triangular.
   * This function is aimed at the case where A is a diagonal block of a
   * larger triangular matrix and B is a horizontal panel of a larger matrix.
   *
   * This object provides functions to pack A into a buffer, and then two
   * functions to solve the problem given that buffer and a buffer
   * where B has been suitably packed. By combining those two solvers with
   * the packing routines, one can cover all the cases.
   *
   * The packing routines for A are related to each other as follow.
   *
   * Let us denote by A'' the matrix obtained by reversing the order of both
   * the rows and the columns of A and let's denote by L (resp. U) a lower
   * (resp. upper) triangular matrix. Then:
   *
   *  - the column slicing of L is equivalent to the column slicing of U''
   *  - the column slicing of U is equivalent to the column slicing of L''
   *  - the row slicing of L is equivalent to the row slicing of U''
   *  - the row slicing of U is equivalent to the row slicing of L''
   *  - the column slicing of L is equivalent to the row slicing of L^T^
   *  - the column slicing of U is equivalent to the row slicing of U^T^
   *  - the row slicing of U is equivalent to the column slicing of U^T^
   *  - the row slicing of L is equivalent to the column slicing of L^T^
   *
   * By "equivalent", we mean that the resulting buffers have the same exact
   * content.
   */
  object TRSBP extends Buffer {

    /**
     * Pack the lower triangular matrix A by slicing thin bands of columns
     *
     * As an illustration, for a 7 x 7 matrix, for kr = 2, the layout is
     *
     * <pre>
     *       0
     *       1  2
     *       3  4 13      ZEROES
     *       5  6 14 15
     *       7  8 16 17 22
     *       9 10 18 19 23 24
     *      11 12 20 21 25 26 27
     * </pre>
     *
     * The diagonal elements stored in the buffer are the inverse of that of
     * the matrix a.
     *
     * Requirement: kr = 2 or kr = 4 but this is not enforced for efficiency
     */
    def packLowerTriangleAsColumnSlices(a:Matrix, hasUnitDiagonal:Boolean,
                                        kr:Int, aa:Long)
    {
      val kc = a.dimensions._1
      val nTri = kr*(kr+1)/2
      val kk = (kc/kr)*kr
      var paa = aa
      var ld = a.ld
      var o = a.start
      var e = a.elements

      cfor(0)(_ < kk, _ + kr) { k =>
        // start of column k and k+1
        val r0 = o + (k+0)*ld
        val r1 = o + (k+1)*ld
        val r2 = o + (k+2)*ld
        val r3 = o + (k+3)*ld
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/e(r0+k))
        put(paa, 1, e(r0+k+1))
        put(paa, 2, if(hasUnitDiagonal) 1.0 else 1.0/e(r1+k+1))
        if(kr == 4) {
          put(paa, 3, e(r0+k+2)); put(paa, 4, e(r1+k+2))
          put(paa, 5, if(hasUnitDiagonal) 1.0 else 1.0/e(r2+k+2))
          put(paa, 6, e(r0+k+3)); put(paa, 7, e(r1+k+3)); put(paa, 8, e(r2+k+3))
          put(paa, 9, if(hasUnitDiagonal) 1.0 else 1.0/e(r3+k+3))
        }
        paa += nTri * 8
        cforRange(k+kr until kc) { p =>
          put(paa, 0, e(r0+p)); put(paa, 1, e(r1+p))
          if(kr == 4) {
          put(paa, 2, e(r2+p)); put(paa, 3, e(r3+p))
          }
          paa += kr * 8
        }
      }
      cforRange(kk until kc) { k =>
        cforRange(kk until k) { p =>
          put(paa, 0, a(k,p))
          paa += 1 * 8
        }
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/a(k,k))
        paa += 1 * 8
      }
    }

    /**
     * Pack the upper triangular matrix A by slicing thin bands of columns
     *
     * As an illustration, for a 7 x 7 matrix, for kr = 2, the layout is
     *
     * <pre>
     *       27 26 25 21 20 12 11
     *          24 23 19 18 10  9
     *             22 17 16  8  7
     *                15 14  6  5
     *                   13  4  3
     *                       2  1
     *                          0
     * </pre>
     *
     * The diagonal elements stored in the buffer are the inverse of that of
     * the matrix a.
     *
     * Requirement: kr = 2 or kr = 4 but this is not enforced for efficiency
     */
    def packUpperTriangleAsColumnSlices(a:Matrix, hasUnitDiagonal:Boolean,
                                        kr:Int, aa:Long)
    {
      val kc = a.dimensions._1
      val nTri = kr*(kr+1)/2
      val kk = kc % kr
      var paa = aa
      var ld = a.ld
      var o = a.start
      var e = a.elements

      cfor(kc-1)(_ > kk, _ - kr) { k =>
        val r0 = o + (k-0)*ld
        val r1 = o + (k-1)*ld
        val r2 = o + (k-2)*ld
        val r3 = o + (k-3)*ld
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/e(r0+k))
        put(paa, 1, e(r0+k-1));
        put(paa, 2, if(hasUnitDiagonal) 1.0 else 1.0/e(r1+k-1))
        if(kr == 4) {
          put(paa, 3, e(r0+k-2)); put(paa, 4, e(r1+k-2))
          put(paa, 5, if(hasUnitDiagonal) 1.0 else 1.0/e(r2+k-2))
          put(paa, 6, e(r0+k-3)); put(paa, 7, e(r1+k-3)); put(paa, 8, e(r2+k-3))
          put(paa, 9, if(hasUnitDiagonal) 1.0 else 1.0/e(r3+k-3))
        }
        paa += nTri * 8
        cforRange(k-kr to 0 by -1) { p =>
          put(paa, 0, e(r0+p)); put(paa, 1, e(r1+p))
          if(kr == 4) {
            put(paa, 2, e(r2+p)); put(paa, 3, e(r3+p))
          }
          paa += kr * 8
        }
      }
      cforRange(kk-1 to 0 by -1) { k =>
        cforRange(kk-1 until k by -1) { p =>
          put(paa, 0, a(k,p))
          paa += 1 * 8
        }
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/a(k,k))
        paa += 1 * 8
      }
    }

    /**
     * Pack the lower triangular matrix A by slicing thin bands of rows
     *
     * As an illustration, for a 7 x 7 matrix, for kr = 2, the layout is
     *
     * <pre>
     *       27
     *       26 24
     *       25 23 22
     *       21 19 17 15
     *       20 18 16 14 13
     *       12 10  8  6  4  2
     *       11  9  7  5  3  1  0
     * </pre>
     *
     * The diagonal elements stored in the buffer are the inverse of that of
     * the matrix a.
     *
     * Requirement: kr = 2 or kr = 4 but this is not enforced for efficiency
     */
    def packLowerTriangleAsRowSlices(a:Matrix, hasUnitDiagonal:Boolean,
                                     kr:Int, aa:Long)
    {
      val kc = a.dimensions._1
      val nTri = kr*(kr+1)/2
      val kk = kc % kr
      var paa = aa
      var ld = a.ld
      var o = a.start
      var e = a.elements

      cfor(kc-1)(_ > kk, _ - kr) { k =>
        val r = o + k + k*ld
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/e(r))
        val r1 = r-ld
        put(paa, 1, e(r1));
        put(paa, 2, if(hasUnitDiagonal) 1.0 else 1.0/e(r1-1))
        if(kr == 4) {
          val r2 = r1-ld
          put(paa, 3, e(r2)); put(paa, 4, e(r2-1));
          put(paa, 5, if(hasUnitDiagonal) 1.0 else 1.0/e(r2-2))
          val r3 = r2-ld
          put(paa, 6, e(r3)); put(paa, 7, e(r3-1)); put(paa, 8, e(r3-2))
          put(paa, 9, if(hasUnitDiagonal) 1.0 else 1.0/e(r3-3))
        }
        paa += nTri * 8
        cforRange(k-kr to 0 by -1) { p =>
          val r = o + k + p*ld
          put(paa, 0, e(r)); put(paa, 1, e(r-1))
          if(kr == 4) {
            put(paa, 2, e(r-2)); put(paa, 3, e(r-3))
          }
          paa += kr * 8
        }
      }
      cforRange(kk-1 to 0 by -1) { k =>
        cforRange(kk-1 until k by -1) { p =>
          put(paa, 0, a(p,k))
          paa += 1 * 8
        }
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/a(k,k))
        paa += 1 * 8
      }
    }

    /**
     * Pack the upper triangular matrix A by slicing thin bands of rows
     *
     * As an illustration, for a 7 x 7 matrix, for kr = 2, the layout is
     *
     * <pre>
     *
     *  0  1  3  5  7  9 11
     *     2  4  6  8 10 12
     *       13 14 16 18 20
     *          15 17 19 21
     *             22 23 25
     *                24 26
     *                   27
     * </pre>
     *
     * The diagonal elements stored in the buffer are the inverse of that of
     * the matrix a.
     *
     * Requirement: kr = 2 or kr = 4 but this is not enforced for efficiency
     */
    def packUpperTriangleAsRowSlices(a:Matrix, hasUnitDiagonal:Boolean,
                                     kr:Int, aa:Long)
    {
      val kc = a.dimensions._1
      val nTri = kr*(kr+1)/2
      val kk = kr*(kc/kr)
      var paa = aa
      var ld = a.ld
      var o = a.start
      var e = a.elements

      cfor(0)(_ < kk, _ + kr) { k =>
        val r = o + k + k*ld
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/e(r))
        val r1 = r+ld
        put(paa, 1, e(r1));
        put(paa, 2, if(hasUnitDiagonal) 1.0 else 1.0/e(r1+1))
        if(kr == 4) {
          val r2 = r1+ld
          put(paa, 3, e(r2)); put(paa, 4, e(r2+1))
          put(paa, 5, if(hasUnitDiagonal) 1.0 else 1.0/e(r2+2))
          val r3 = r2+ld
          put(paa, 6, e(r3));put(paa, 7, e(r3+1)); put(paa, 8, e(r3+2))
          put(paa, 9, if(hasUnitDiagonal) 1.0 else 1.0/e(r3+3))
        }
        paa += nTri * 8
        cforRange(k+kr until kc) { p =>
          val r = o + k + p*ld
          put(paa, 0, e(r)); put(paa, 1, e(r+1))
          if(kr == 4) {
            put(paa, 2, e(r+2)); put(paa, 3, e(r+3))
          }
          paa += kr * 8
        }
      }
      cforRange(kk until kc) { k =>
        cforRange(kk until k) { p =>
          put(paa, 0, a(p,k))
          paa += 1 * 8
        }
        put(paa, 0, if(hasUnitDiagonal) 1.0 else 1.0/a(k,k))
        paa += 1 * 8
      }
    }

    /**
     * Solve L X = B where L is lower triangular
     *
     * L is kc x kc and shall have been packed in the buffer starting at aa
     * by the equivalent of packLowerTriangleAsColumnSlices with kr = 2
     * whereas B is a kc x n general matrix that shall have been packed
     * in the buffer starting at bb by the equivalent of GEBP.packColumnSlices
     * using the given value of nr.
     *
     * Requirement: nr == 4. Not enforced for performance reason,
     * i.e. using any other value will lead to disaster.
     */
    def solveLowerFromLeft(kc:Int, n:Int, nr:Int, hasUnitDiagonal:Boolean,
                           aa:Long, bb:Long)
    {
      // B is kc x n and A is kc x kc
      val k2 = (kc/2)*2
      val n4 = (n/4)*4
      var paa = aa

      cforRange(0 until k2 by 2) { k =>
        // Solve A(k:k+2, k:k+2) X(k:k+2, :) = B(k:k+2, :)
        // then update B(k+2:kc, :) -= A(k+2:kc, k:k+2) X(k:k+2, :)
        val t0 = get(paa, 0); val t1 = get(paa, 1); val t2 = get(paa, 2)
        paa += 3 * 8

        cforRange(0 until n4 by 4) { j =>
          // Solve A(k:k+2, k:k+2) X(k:k+2, j:j+4) = B(k:k+2, j:j+4),
          //   [ 1/t0    0  ] [ x0 x1 x2 x3 ] = [ b0 b1 b2 b3 ]
          //   [  t1   1/t2 ] [ x4 x5 x6 x7 ]   [ b4 b5 b6 b7 ]
          // Hopefully, this will stay in registers:
          var pbb = bb + (nr*k + j*kc) * 8 // location of B(k, j)
          var x0 = get(pbb, 0); var x1 = get(pbb, 1)
          var x2 = get(pbb, 2); var x3 = get(pbb, 3)
          var x4 = get(pbb, 4); var x5 = get(pbb, 5)
          var x6 = get(pbb, 6); var x7 = get(pbb, 7)
          x0 *= t0; x4 -= t1*x0; x4 *= t2
          x1 *= t0; x5 -= t1*x1; x5 *= t2
          x2 *= t0; x6 -= t1*x2; x6 *= t2
          x3 *= t0; x7 -= t1*x3; x7 *= t2
          put(pbb, 0, x0); put(pbb, 1, x1)
          put(pbb, 2, x2); put(pbb, 3, x3)
          put(pbb, 4, x4); put(pbb, 5, x5)
          put(pbb, 6, x6); put(pbb, 7, x7)

          pbb += 8 * 8

          var qaa = paa
          cforRange(k+2 until kc) { p =>
            // Update B(p, j:j+4) -= A(p, k:k+2) X(k:k+2, j:j+4),
            //   [ b0 b1 b2 b3 ] -= [ a0 a1 ] [ x0 x1 x2 x3 ]
            //                                [ x4 x5 x6 x7 ]
            val a0 = get(qaa, 0); val a1 = get(qaa, 1)
            var b0 = get(pbb, 0); var b1 = get(pbb, 1)
            var b2 = get(pbb, 2); var b3 = get(pbb, 3)
            b0 -= a0*x0 + a1*x4
            b1 -= a0*x1 + a1*x5
            b2 -= a0*x2 + a1*x6
            b3 -= a0*x3 + a1*x7
            put(pbb, 0, b0); put(pbb, 1, b1)
            put(pbb, 2, b2); put(pbb, 3, b3)

            qaa += 2 * 8
            pbb += 4 * 8
          }
        }
        cforRange(n4 until n) { j =>
          // Solve A(k:k+2, k:k+2) X(k:k+2, j) = B(k:k+2, j),
          //   [ 1/t0    0  ] [ x0 ] = [ b0 ]
          //   [  t1   1/t2 ] [ x1 ]   [ b1 ]
          var pbb = bb + (k + j*kc) * 8 // location of B(k, j)
          var x0 = get(pbb, 0); var x1 = get(pbb, 1)
          x0 *= t0; x1 -= t1*x0; x1 *= t2
          put(pbb, 0, x0); put(pbb, 1, x1)

          pbb += 2 * 8

          var qaa = paa
          cforRange(k+2 until kc) { p =>
            // Update B(p, j) -= A(p, k:k+2) X(k:k+2, j),
            //   [ b0 ] -= [ a0 a1 ] [ x0 ]
            //                       [ x1 ]
            val a0 = get(qaa, 0); val a1 = get(qaa, 1)
            var b0 = get(pbb, 0)
            b0 -= a0*x0 + a1*x1
            put(pbb, 0, b0)

            qaa += 2 * 8
            pbb += 1 * 8
          }
        }

        paa += 2*(kc - k - 2) * 8
      }
      if(k2 != kc) {
        val t0 = get(paa, 0)
        cforRange(0 until n4 by 4) { j =>
          // Solve A(k2,k2) X(k2, j:j+4) = B(k2, j:j+4)
          //   [ 1/t0 ] [ x0 x1 x2 x3 ] = [ b0 b1 b2 b3]
          var pbb = bb + (nr*k2 + j*kc) * 8 // location of B(k2, j)
          var x0 = get(pbb, 0); var x1 = get(pbb, 1)
          var x2 = get(pbb, 2); var x3 = get(pbb, 3)
          x0 *= t0; x1 *= t0; x2 *= t0; x3 *= t0
          put(pbb, 0, x0); put(pbb, 1, x1)
          put(pbb, 2, x2); put(pbb, 3, x3)
        }
        cforRange(n4 until n) { j =>
          var pbb = bb + (k2 + j*kc) * 8 // location of B(k2, j)
          // Solve A(k2,k2) X(k2,n2) = B(k2,n2),
          //   [ 1/t0 ] [ x0 ] = [ b0 ]
          var x0 = get(pbb, 0)
          x0 *= t0
          put(pbb, 0, x0)
        }
      }
    }

    /**
     * Solve X U = B where U is upper triangular
     *
     * U is kc x kc and shall have been packed in the buffer starting at aa
     * by the equivalent of packUpperTriangleAsRowSlices with kr = 4
     * whereas B is a m x kc general matrix that shall have been packed
     * in the buffer starting at bb by the equivalent of GEBP.packRowSlices
     * using the given value of mr.
     *
     * Requirement: mr == 2. Not enforced for performance reason,
     * i.e. using any other value will lead to disaster.
     */
    def solveUpperFromRight(kc:Int, m:Int, mr:Int, hasUnitDiagonal:Boolean,
                            aa:Long, bb:Long)
    {
      // B is m x kc and A is kc x kc
      val k4 = (kc/4)*4
      val r4 = kc - k4
      val m2 = (m/2)*2
      var paa = aa

      cforRange(0 until k4 by 4) { k =>
        // Solve X(:, k:k+4) A(k:k+4, k:k+4) = B(:, k:k+4)
        // then update B(:, k+4:kc) -= X(:, k:k+4) A(k:k+4, k+4:kc)
        val t0 = get(paa, 0); val t1 = get(paa, 1); val t2 = get(paa, 2)
        val t3 = get(paa, 3); val t4 = get(paa, 4); val t5 = get(paa, 5)
        val t6 = get(paa, 6); val t7 = get(paa, 7); val t8 = get(paa, 8)
        val t9 = get(paa, 9)
        paa += 10 * 8

        cforRange(0 until m2 by 2) { i =>
          // Solve X(i:i+2, k:k+4) A(k:k+4, k:k+4) = B(i:i+2, k:k+4),
          //   [ x0 x2 x4 x6 ] [ 1/t0 t1    t3    t6   ] = [ b0 b2 b4 b6 ]
          //   [ x1 x3 x5 x7 ] [      1/t2  t4    t7   ]   [ b1 b3 b5 b7 ]
          //                   [            1/t5  t8   ]
          //                   [                  1/t9 ]
          var pbb = bb + (k*mr + i*kc) * 8 //location of B(i, k)
          var x0 = get(pbb, 0); var x2 = get(pbb, 2)
          var x4 = get(pbb, 4); var x6 = get(pbb, 6)
          var x1 = get(pbb, 1); var x3 = get(pbb, 3)
          var x5 = get(pbb, 5); var x7 = get(pbb, 7)

          x0 *= t0
          x2 -= t1*x0; x2 *= t2
          x4 -= t3*x0; x4 -= t4*x2; x4 *= t5
          x6 -= t6*x0; x6 -= t7*x2; x6 -= t8*x4; x6 *= t9

          x1 *= t0
          x3 -= t1*x1; x3 *= t2
          x5 -= t3*x1; x5 -= t4*x3; x5 *= t5
          x7 -= t6*x1; x7 -= t7*x3; x7 -= t8*x5; x7 *= t9

          put(pbb, 0, x0); put(pbb, 2, x2)
          put(pbb, 4, x4); put(pbb, 6, x6)
          put(pbb, 1, x1); put(pbb, 3, x3)
          put(pbb, 5, x5); put(pbb, 7, x7)

          pbb += 8 * 8

          var qaa = paa
          cforRange(k+4 until kc) { p =>
            // Update B(i:i+2, p) -= X(i:i+2, k:k+4) A(k:k+4, p)
            //   [ b0 ] -= [ x0 x2 x4 x6 ] [ a0 ]
            //   [ b1 ]    [ x1 x3 x5 x7 ] [ a1 ]
            //                             [ a2 ]
            //                             [ a3 ]
            val a0 = get(qaa, 0); val a1 = get(qaa, 1)
            val a2 = get(qaa, 2); val a3 = get(qaa, 3)
            var b0 = get(pbb, 0); var b1 = get(pbb, 1)
            b0 -= x0*a0 + x2*a1 + x4*a2 + x6*a3
            b1 -= x1*a0 + x3*a1 + x5*a2 + x7*a3
            put(pbb, 0, b0); put(pbb, 1, b1)

            qaa += 4 * 8
            pbb += 2 * 8
          }
        }
        if(m2 != m) {
          // Solve X(m2, k:k+4) A(k:k+4, k:k+4) = B(m2, k:k+4)
          //   [ x0 x1 x2 x3 ] [ 1/t0 t1    t3    t6   ] = [ b0 b1 b2 b3 ]
          //                   [      1/t2  t4    t7   ]
          //                   [            1/t5  t8   ]
          //                   [                  1/t9 ]
          var pbb = bb + (k + m2*kc) * 8 // location of B(m2, k)
          var x0 = get(pbb, 0); var x1 = get(pbb, 1)
          var x2 = get(pbb, 2); var x3 = get(pbb, 3)
          x0 *= t0
          x1 -= t1*x0; x1 *= t2
          x2 -= t3*x0; x2 -= t4*x1; x2 *= t5
          x3 -= t6*x0; x3 -= t7*x1; x3 -= t8*x2; x3 *= t9
          put(pbb, 0, x0); put(pbb, 1, x1)
          put(pbb, 2, x2); put(pbb, 3, x3)

          pbb += 4 * 8

          var qaa = paa
          cforRange(k+4 until kc) { p =>
            // Update B(m2, p) -= X(m2, k:k+4) A(k:k+4, p)
            //   [ b0 ] -= [ x0 x1 x2 x3 ] [ a0 ]
            //                             [ a1 ]
            //                             [ a2 ]
            //                             [ a3 ]
            val a0 = get(qaa, 0); val a1 = get(qaa, 1)
            val a2 = get(qaa, 2); val a3 = get(qaa, 3)
            var b0 = get(pbb, 0)
            b0 -= a0*x0 + a1*x1 + a2*x2 + a3*x3
            put(pbb, 0, b0)

            qaa += 4 * 8
            pbb += 1 * 8
          }
        }

        paa += 4*(kc - k - 4) * 8
      }

      // Solve X(:, k4:kc) A(k4:kc, k4:kc) = B(:, k4:kc) if need be
      if(k4 != kc) {
        // r4 is 1, 2 or 3
        val t0 = get(paa, 0)
        val (t1, t2) = if(r4 > 1) (get(paa, 1), get(paa, 2)) else (0.0, 0.0)
        val (t3, t4, t5) = if(r4 > 2) (get(paa, 3), get(paa, 4), get(paa,5))
                           else (0.0, 0.0, 0.0)
        cforRange(0 until m2 by 2) { i =>
          // Solve X(i:i+2, k4:kc) A(k4:kc, k4:kc) = B(i:i+2, k4:kc)
          // Biggest is
          // [ x0 x2 x4 ] [ 1/t0  t1   t3  ] = [ b0 b2 b4 ]
          // [ x1 x3 x5 ] [      1/t2  t4  ]   [ b1 b3 b5 ]
          //              [           1/t5 ]
          //
          val pbb = bb + (mr*k4 + i*kc) * 8 // location of B(i, k4)
          var x0 = get(pbb, 0); var x1 = get(pbb, 1)
          x0 *= t0; x1 *= t0
          put(pbb, 0, x0); put(pbb, 1, x1)
          if(r4 > 1) {
            var x2 = get(pbb, 2); var x3 = get(pbb, 3)
            x2 -= t1*x0; x3 -= t1*x1
            x2 *= t2; x3 *= t2
            put(pbb, 2, x2); put(pbb, 3, x3)
            if(r4 > 2) {
              var x4 = get(pbb, 4); var x5 = get(pbb, 5)
              x4 -= t3*x0 + t4*x2; x5 -= t3*x1 + t4*x3
              x4 *= t5; x5 *= t5
              put(pbb, 4, x4); put(pbb, 5, x5)
            }
          }
        }
        if(m2 != m) {
          // Solve X(m2, k4:kc) A(k4:kc, k4:kc) = B(m4, k4:kc)
          // Biggest is
          // [ x0 x1 x2 ] [ 1/t0  t1   t3  ] = [ b0 b1 b2 ]
          //              [      1/t2  t4  ]
          //              [           1/t5 ]
          val pbb = bb + (k4 + m2*kc) * 8 // location of B(m2, k4)
          var x0 = get(pbb, 0)
          x0 *= t0
          put(pbb, 0, x0)
          if(r4 > 1) {
            var x1 = get(pbb, 1)
            x1 -= t1*x0
            x1 *= t2
            put(pbb, 1, x1)
            if(r4 > 2) {
              var x2 = get(pbb, 2)
              x2 -= t3*x0 + t4*x1
              x2 *= t5
              put(pbb, 2, x2)
            }
          }
        }
      }
    }
  }

  /**
   * @inheritdoc
   *
   * '''Algorithm'''
   *
   * We have a priori 8 cases to handle. With a clever choice of packing
   * routines, we narrow it down to only two cases of forward substitution
   * by block,
   *   1. L X = B,
   *   2. X U = B,
   * where L (resp. U) denotes a lower (resp. upper) triangular matrix.
   *
   * In case (1), the algorithm decomposes the problem as follow
   *
   * <pre>
   * [ L11  0  ] [ X1 ] = [ B1 ]
   * [ L21 L22 ] [ X2 ]   [ B2 ]
   * </pre>
   *
   * where L11 is kc x kc, where kc is chosen to fit well with processor caches.
   * First L11 X1 = B1 is solved with the appropriate kernel from TRSBP,
   * with L11 packed as column slices, and B1 as well, overwriting B1 with X1.
   * Then the update B2 -= L21 X1 is performed using a sequence of GEBP,
   * with blocks from L21 packed as row slices. One then recurses by solving
   * L22 X22 = B2.
   *
   * Case (2) is similar, with a decomposition
   *
   * <pre>
   * [ X1 X2 ] [ U11 U12 ] = [ B1 B2 ]
   *           [  0  U22 ]
   * </pre>
   *
   * and solving first X1 U11 = B1 with TRSBP, with U11 packed as row slices,
   * and B1 as well, then updating B2 -= X1 U12 with a sequence of GEBP,
   *  with U12 blocks packed as column slices, and recursing with X2 U22 = B2.
   *
   * In both cases, there is a tail recursion, which is therefore implemented
   * as a loop. Moreover the packing routines are those featured by
   * object TRSBP and GEBP.
   *
   * The 6 other cases admit similar decompositions, leading to either forward
   * or backward substitution.
   *
   *   3. U X = B: backward substitution. The algorithm proceeds as:
   *      solve U22 X2 = B2, update B1 -= U12 X2, recurse with U11 X1 = B1.
   *      The recipe is:
   *      pack U22 as column slices, pack B2 as column slices with rows in
   *      reverse order, use the TRSBP kernel for LX=B, pack U12 blocks as
   *      row slices with columns in reverse order;
   *   4. X L = B: backward substitution. The algorithm proceeds as:
   *      solve X2 L22 = B2, update B1 -= X2 L21, recurse with X1 L11 = B1.
   *      The recipe is:
   *      pack L22 as row slices, pack B2 as row slices with columns in reverse
   *      order, use the TRSBP kernel for XU=B, pack L21 as column slices with
   *      rows in reverse order;
   *   5. L^T^ X = B: backward substitution. The algorithm proceeds as:
   *      solve L22^T^ X2 = B2, update B1 -= L21^T^ X2, recurse
   *      with L11^T^ X1 = B1. The recipe is:
   *      pack L22 as row slices, pack B2 as column slices with rows in
   *      reverse order, use the TRSBP kernel for LX=B, pack L21 blocks as
   *      column slices with rows in reverse order;
   *   6. X U^T^ = B: backward substitution. The algorithm proceeds as:
   *      solve X2 U22^T^ = B2, update B1 -= X2 U12^T^,
   *      recurse with X1 L11^T^ = B1. The recipe is:
   *      pack U22 as column slices, pack B2 as row slices with columns in
   *      reverse order, use the TRSBP kernel for XU=B, pack L21 as row
   *      slices with columns in reverse order;
   *   7. X L^T^ = B: forward substitution. The algorithm proceeds as:
   *      solve X1 L11^T^ = B1, update B2 -= X1 L21^T^, recurse
   *      with X2 L22^T^ = B2. The recipe is:
   *      pack L11 as column slices, pack B1 as row slices, use the TRSBP
   *      kernel for XU=B, pack L21 blocks as row slices;
   *   8. U^T^ X = B: forward substitution. The algorithm proceeds as:
   *      solve U11^T^ X1 = B1, update B2 -= U12^T^ X1, recurse with
   *      U22^T^ X2 = B2. The recipe is:
   *      pack U11 as row slices, pack B1 as column slices, use the TRSBP
   *      kernel for LX=B, pack U12 blocks as column slices.
   *
   *
   * Let us demonstrate these rules. We will denote by P the permutation matrix
   * such that for any matrix A, PA is A with its rows in reverse order
   * and AP is A with its columns in reverse order. Obviously P^2^ is the
   * identity matrix and P^T = P.
   *
   * Case (3) is equivalent to (P U22 P) (P X2) = P B2. By packing U22 as column
   * slices, the lower triangular matrix L22 = P U22 P is packed as column
   * slices too, thanks to the equivalences built in the packing routines of
   * TRSBP. So we can use the LX=B kernel, providing we change the order of
   * the rows of B2 (and X2). Then the update is equivalent to
   * B1 -= (U12 P) (P X2), hence the change of the order of the columns of U12.
   * QED.
   *
   * For case (8), again using the equivalence between the packing routines,
   * packing U11 as row slices results in U11^T^ being packed as column slices,
   * which makes it possible to use the LX=B kernel from TRSBP. The packing of
   * U12 is then just what is mandated by GEBP.
   * QED.
   *
   * The demonstration for the other cases are mere variations of the same
   * arguments.
   */
  def trsm(side:Sides.Value, uplo:UpperOrLower.Value,
           trans:Transposition.Value, diag:DiagonalProperty.Value,
           alpha:Double, a:Matrix, b:Matrix) {
    checkTrsmPreconditions(side, uplo, trans, diag, alpha, a, b)

    val (m, n) = b.dimensions

    val blocking = GEBP.threadLocalBlocking.get()
    val mc = blocking.mc
    val kc = blocking.kc
    val mr = blocking.mr
    val nr = blocking.nr

    // Trivial cases: quick return
    if(m == 0 || n == 0) return
    if(alpha == 0) {
      b := 0
      return
    }

    // Charge!
    require(nr == 4)
    require(kc <= mc)

    val aa = blocking.bufferA

    val hasUnitDiagonal = diag == UnitDiagonal

    // Process the 8 cases
    // The clearest code we could write incurs writing a slightly different
    // version of the two imbricated loops over the blocks for each case.
    (side, uplo, trans) match {
      // 1. L X = B
      case (FromLeft, Lower, NoTranspose) => {
        val kr = 2
        val bb = blocking.bufferB(n)
        cfor(0)(_ < m, _ + kc) { k =>
          val kc1 = math.min(m - k, kc)
          // Solve L(k:k+kc1, k:k+kc1) X(k:k+kc1, :) = B(k:k+kc1, :)
          TRSBP.packLowerTriangleAsColumnSlices(a.block(k,k+kc1)(k,k+kc1),
                                                hasUnitDiagonal, kr, aa)
          GEBP.packColumnSlices(false)(b.block(k,k+kc1)(0,n), nr, bb)
          TRSBP.solveLowerFromLeft(kc1, n, nr, hasUnitDiagonal, aa, bb)

          // B(k+kc1:m, :) -= L(k+kc1:m, k:k+kc1) X(k:k+kc1, :)
          cfor(k+kc1)(_ < m, _ + mc) { i =>
            val mc1 = math.min(m - i, mc)
            GEBP.packRowSlices(false)(a.block(i,i+mc1)(k,k+kc1), mr, aa)
            GEBP(kc1, mr, nr, -1.0, aa, bb, b.block(i,i+mc1)(0,n))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackColumnSlices(false)(alpha, bb, nr, b.block(k,k+kc1)(0,n))
        }
      }
      // 2. X U = B
      case (FromRight, Upper, NoTranspose) => {
        val kr = 4
        val bb = blocking.bufferB(m)
        cfor(0)(_ < n, _ + kc) { k =>
          val kc1 = math.min(n - k, kc)
          // Solve X(:, k:k+kc1) U(k:k+kc1, k:k+kc1) = B(:, k:k+kc1)
          TRSBP.packUpperTriangleAsRowSlices(a.block(k,k+kc1)(k,k+kc1),
                                             hasUnitDiagonal, kr, aa)
          GEBP.packRowSlices(false)(b.block(0,m)(k,k+kc1), mr, bb)
          TRSBP.solveUpperFromRight(kc1, m, mr, hasUnitDiagonal, aa, bb)

          // B(:, k+kc1:n) -= X(:, k:k+kc1) U(k:k+kc1, k+kc1:n)
          cfor(k+kc1)(_ < n, _ + mc) { i =>
            val mc1 = math.min(n - i, mc)
            GEBP.packColumnSlices(false)(a.block(k,k+kc1)(i,i+mc1), nr, aa)
            GEBP(kc1, mr, nr, -1.0, bb, aa, b.block(0,m)(i,i+mc1))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackRowSlices(false)(alpha, bb, mr, b.block(0,m)(k,k+kc1))
        }
      }
      // 3. U X = B
      case (FromLeft, Upper, NoTranspose) => {
        val kr = 2
        val bb = blocking.bufferB(n)
        cfor(m)(_ > 0, _ - kc) { k =>
          val kc1 = math.min(k, kc)
          // Solve U(k-kc1:k, k-kc1:k) X(k-kc1:k, :) = B(k-kc1:k, :)
          TRSBP.packUpperTriangleAsColumnSlices(a.block(k-kc1,k)(k-kc1,k),
                                                hasUnitDiagonal, kr, aa)
          GEBP.packColumnSlices(true)(b.block(k-kc1,k)(0,n), nr, bb)
          TRSBP.solveLowerFromLeft(kc1, n, nr, hasUnitDiagonal, aa, bb)

          // B(0:k-kc1, :) -= U(0:k-kc1, k-kc1:k) X(k-kc1:k, :)
          cfor(k-kc1)(_ > 0, _ - mc) { i =>
            val mc1 = math.min(i, mc)
            GEBP.packRowSlices(true)(a.block(i-mc1,i)(k-kc1,k), mr, aa)
            GEBP(kc1, mr, nr, -1.0, aa, bb, b.block(i-mc1,i)(0,n))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackColumnSlices(true)(alpha, bb, nr, b.block(k-kc1,k)(0,n))
        }
      }
      // 4. X L = B
      case (FromRight, Lower, NoTranspose) => {
        val kr = 4
        val bb = blocking.bufferB(m)
        cfor(n)(_ > 0, _ - kc) { k =>
          val kc1 = math.min(k, kc)
          // Solve X(:, k-kc1:k) L(k-kc1:k, k-kc1:k) = B(:, k-kc1:k)
          TRSBP.packLowerTriangleAsRowSlices(a.block(k-kc1,k)(k-kc1,k),
                                             hasUnitDiagonal, kr, aa)
          GEBP.packRowSlices(true)(b.block(0,m)(k-kc1,k), mr, bb)
          TRSBP.solveUpperFromRight(kc1, m, mr, hasUnitDiagonal, aa, bb)

          // B(:, 0:k-kc1) -= X(:, k-kc1:k) L(k-kc1:k, 0:k-kc1)
          cfor(k-kc1)(_ > 0, _ - mc) { i =>
            val mc1 = math.min(i, mc)
            GEBP.packColumnSlices(true)(a.block(k-kc1,k)(i-mc1,i), nr, aa)
            GEBP(kc1, mr, nr, -1.0, bb, aa, b.block(0,m)(i-mc1,i))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackRowSlices(true)(alpha, bb, mr, b.block(0,m)(k-kc1,k))
        }
      }
      // 5. L^T X = B
      case (FromLeft, Lower, Transpose) => {
        val kr = 2
        val bb = blocking.bufferB(n)
        cfor(m)(_ > 0, _ - kc) { k =>
          val kc1 = math.min(k, kc)
          // Solve L(k-kc1:k, k-kc1:k)^T X(k-kc1:k, :) = B(k-kc1:k, :)
          TRSBP.packLowerTriangleAsRowSlices(a.block(k-kc1,k)(k-kc1,k),
                                             hasUnitDiagonal, kr, aa)
          GEBP.packColumnSlices(true)(b.block(k-kc1,k)(0,n), nr, bb)
          TRSBP.solveLowerFromLeft(kc1, n, nr, hasUnitDiagonal, aa, bb)

          // B(0:k-kc1, :) -= L(k-kc1:k, 0:k-kc1)^T X(k-kc1:k, :)
          cfor(k-kc1)(_ > 0, _ - mc) { i =>
            val mc1 = math.min(i, mc)
            GEBP.packColumnSlices(true)(a.block(k-kc1,k)(i-mc1,i), mr, aa)
            GEBP(kc1, mr, nr, -1.0, aa, bb, b.block(i-mc1,i)(0,n))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackColumnSlices(true)(alpha, bb, nr, b.block(k-kc1,k)(0,n))
        }
      }
      // 6. X U^T = B
      case (FromRight, Upper, Transpose) => {
        val kr = 4
        val bb = blocking.bufferB(m)
        cfor(n)(_ > 0, _ - kc) { k =>
          val kc1 = math.min(k, kc)
          // Solve X(:, k-kc1:k) U(k-kc1:k, k-kc1:k)^T = B(:, k-kc1:k)
          TRSBP.packUpperTriangleAsColumnSlices(a.block(k-kc1,k)(k-kc1,k),
                                                hasUnitDiagonal, kr, aa)
          GEBP.packRowSlices(true)(b.block(0,m)(k-kc1,k), mr, bb)
          TRSBP.solveUpperFromRight(kc1, m, mr, hasUnitDiagonal, aa, bb)

          // B(:, 0:k-kc1) -= X(:, k-kc1:k) U(0:k-kc1, k-kc1:k)^T
          cfor(k-kc1)(_ > 0, _ - mc) { i =>
            val mc1 = math.min(i, mc)
            GEBP.packRowSlices(true)(a.block(i-mc1,i)(k-kc1,k), nr, aa)
            GEBP(kc1, mr, nr, -1.0, bb, aa, b.block(0,m)(i-mc1,i))
          }

          // unpack B(k-kc1:k, :) from bb
          GEBP.unpackRowSlices(true)(alpha, bb, mr, b.block(0,m)(k-kc1,k))
        }
      }
      // 7. X L^T = B
      case (FromRight, Lower, Transpose) => {
        val kr = 4
        val bb = blocking.bufferB(m)
        cfor(0)(_ < n, _ + kc) { k =>
          val kc1 = math.min(n - k, kc)
          // Solve X(:, k:k+kc1) L(k:k+kc1, k:k+kc1)^T = B(:, k:k+kc1)
          TRSBP.packLowerTriangleAsColumnSlices(a.block(k,k+kc1)(k,k+kc1),
                                                hasUnitDiagonal, kr, aa)
          GEBP.packRowSlices(false)(b.block(0,m)(k,k+kc1), mr, bb)
          TRSBP.solveUpperFromRight(kc1, m, mr, hasUnitDiagonal, aa, bb)

          // B(:, k+kc1:n) -= X(:, k:k+kc1) L(k+kc1:n, k:k+kc1)^T
          cfor(k+kc1)(_ < n, _ + mc) { i =>
            val mc1 = math.min(n - i, mc)
            GEBP.packRowSlices(false)(a.block(i,i+mc1)(k,k+kc1), nr, aa)
            GEBP(kc1, mr, nr, -1.0, bb, aa, b.block(0,m)(i,i+mc1))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackRowSlices(false)(alpha, bb, mr, b.block(0,m)(k,k+kc1))
        }
      }
      // 8. U^T X = B
      case (FromLeft, Upper, Transpose) => {
        val kr = 2
        val bb = blocking.bufferB(n)
        cfor(0)(_ < m, _ + kc) { k =>
          val kc1 = math.min(m - k, kc)
          // Solve U(k:k+kc1, k:k+kc1)^T X(k:k+kc1, :) = B(k:k+kc1, :)
          TRSBP.packUpperTriangleAsRowSlices(a.block(k,k+kc1)(k,k+kc1),
                                             hasUnitDiagonal, kr, aa)
          GEBP.packColumnSlices(false)(b.block(k,k+kc1)(0,n), nr, bb)
          TRSBP.solveLowerFromLeft(kc1, n, nr, hasUnitDiagonal, aa, bb)

          // B(k+kc1:m, :) -= U(k:k+kc1, k+kc1:m)^T X(k:k+kc1, :)
          cfor(k+kc1)(_ < m, _ + mc) { i =>
            val mc1 = math.min(m - i, mc)
            GEBP.packColumnSlices(false)(a.block(k,k+kc1)(i,i+mc1), mr, aa)
            GEBP(kc1, mr, nr, -1.0, aa, bb, b.block(i,i+mc1)(0,n))
          }

          // unpack B(k:k+kc1, :) from bb
          GEBP.unpackColumnSlices(false)(alpha, bb, nr, b.block(k,k+kc1)(0,n))
        }
      }

    }
  }
}

/** Convenience object to enable `import` instead of `extends` */
object LayeredLevel3 extends LayeredLevel3



