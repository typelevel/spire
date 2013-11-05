package spire.matrix.dense.BLAS

import spire.syntax.cfor._

import spire.matrix.dense.MatrixLike
import spire.matrix.{Transposition, UpperOrLower}
import Transposition._
import UpperOrLower._

/**
 * Straightforward but inefficient implementations
 *
 * No blocking, no parallelism, no vectorisation(?).
 */
trait NaiveLevel3 extends Level3 {

  def gemm(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:MatrixLike, b:MatrixLike,
           beta:Double, c:MatrixLike): Unit = {
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

    val (m, n) = c.dimensions
    val k = if(transB == NoTranspose) b.dimensions._1 else b.dimensions._2

    // trivial cases
    if(alpha == 0) {
      if(beta == 0)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) = 0 }
      else if(beta != 1)
        cforRange2(0 until n, 0 until m) { (j,i) => c(i,j) *= beta }
      return
    }

    // charge!
    if (transB == NoTranspose) {
      if(transA == NoTranspose) {
        // (I) C := alpha A B + beta C
        cforRange(0 until n) { j =>
          if(beta == 0)
            cforRange(0 until m) { i => c(i,j) = 0 }
          else if(beta != 1)
            cforRange(0 until m) { i => c(i,j) *= beta }
          cforRange(0 until k) { l =>
            if(b(l,j) != 0) {
              val t = alpha*b(l,j)
              cforRange(0 until m) { i => c(i,j) += t*a(i,l) }
            }
          }
        }
      }
      else {
        // (II) C := alpha A^T B + beta C
        cforRange2(0 until n, 0 until m) { (j, i) =>
          var t = 0.0
          cforRange(0 until k) { l => t += a(l,i)*b(l,j) }
          if(beta == 0) c(i,j) = alpha*t
          else          c(i,j) = alpha*t + beta*c(i,j)
        }
      }
    }
    else {
      if(transA == NoTranspose) {
        // (III) alpha A B^T + beta C,
        // same as (I) except that b(l,j) becomes b(j,l)
        cforRange(0 until n) { j =>
          if(beta == 0)
            cforRange(0 until m) { i => c(i,j) = 0 }
          else if(beta != 1)
            cforRange(0 until m) { i => c(i,j) *= beta }
          cforRange(0 until k) { l =>
            if(b(j,l) != 0) {
              val t = alpha*b(j,l)
              cforRange(0 until m) { i => c(i,j) += t*a(i,l) }
            }
          }
        }
      }
      else {
        // (IV) alpha A^T B^T + beta C,
        // same as (II) except that b(l,j) becomes b(j,l)
        cforRange2(0 until n, 0 until m) { (j, i) =>
          var t = 0.0
          cforRange(0 until k) { l => t += a(l,i)*b(j,l) }
          if(beta == 0) c(i,j) = alpha*t
          else          c(i,j) = alpha*t + beta*c(i,j)
        }
      }
    }

  }

  def syrk(uplo:UpperOrLower.Value, trans:Transposition.Value,
           alpha:Double, a:MatrixLike, beta:Double, c:MatrixLike): Unit = {
    require(c.isSquare)
    if(trans == NoTranspose)
      require(c.dimensions._1 ==  a.dimensions._1)
    else
      require(c.dimensions._1 ==  a.dimensions._2)

    val n = c.dimensions._1
    val k = if(trans == NoTranspose) a.dimensions._2 else a.dimensions._1
    if(n==0 || ((alpha==0 || k==0) && beta==1)) return

    if(alpha == 0) {
      if(uplo == Upper)
        cforRange(0 until n) { j =>
          cforRange(0 to j) { i =>
            c(i,j) *= beta
          }
        }
      else
        cforRange(0 until n) { j =>
          cforRange(j until n) { i =>
            c(i,j) *= beta
          }
        }
      return
    }

    if(trans == NoTranspose) {
      // C = alpha A A^T + beta C
      if(uplo == Upper) {
        cforRange(0 until n) { j =>
          cforRange(0 to j) { i => c(i,j) *= beta }
          cforRange(0 until k) { l =>
            if(a(j,l) != 0) {
              val t = alpha*a(j,l)
              for(i <- 0 to j) c(i,j) += t*a(i,l)
            }
          }
        }
      }
      else {
        cforRange(0 until n) { j =>
          cforRange(j until n) { i => c(i,j) *= beta }
          cforRange(0 until k) { l =>
            if(a(j,l) != 0) {
              val t = alpha*a(j,l)
              cforRange(j until n) { i => c(i,j) += t*a(i,l) }
            }
          }
        }
      }
    }
    else {
      // C = alpha A^T A + beta C
      if(uplo == Upper) {
        cforRange(0 until n) { j => cforRange(0 to j) { i =>
          var t = 0.0
          cforRange(0 until k) { l => t += a(l,i)*a(l,j) }
          c(i,j) = alpha*t + beta*c(i,j)
        }}
      }
      else {
        cforRange(0 until n) { j => cforRange(j until n) { i =>
          var t = 0.0
          cforRange(0 until k) { l => t += a(l,i)*a(l,j) }
          c(i,j) = alpha*t + beta*c(i,j)
        }}
      }
    }
  }
}

