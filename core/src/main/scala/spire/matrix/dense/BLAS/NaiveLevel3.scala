package spire.matrix.dense.BLAS

import spire.syntax.cfor._

import spire.matrix.dense.Matrix
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Sides._
import Transposition._
import UpperOrLower._
import DiagonalProperty._

/**
 * Straightforward but inefficient implementations
 *
 * No blocking, no parallelism, no vectorisation(?).
 */
trait NaiveLevel3 extends Level3 {

  def gemm(transA:Transposition.Value, transB:Transposition.Value,
           alpha:Double, a:Matrix, b:Matrix,
           beta:Double, c:Matrix): Unit = {
    checkGemmPreconditions(transA, transB, alpha, a, b, beta, c)

    // trivial case
    if(alpha == 0) {
      trivialGemm(transA, transB, a, b, beta, c)
      return
    }

    // charge!
    val (m, n) = c.dimensions
    val k = if(transB == NoTranspose) b.dimensions._1 else b.dimensions._2
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
           alpha:Double, a:Matrix, beta:Double, c:Matrix): Unit = {
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

  def trsm(side:Sides.Value, uplo:UpperOrLower.Value,
           trans:Transposition.Value, diag:DiagonalProperty.Value,
           alpha:Double, a:Matrix, b:Matrix) {
    checkTrsmPreconditions(side, uplo, trans, diag, alpha, a, b)

    val (m, n) = b.dimensions

    // Trivial cases: quick return
    if(m == 0 || n == 0) return
    if(alpha == 0) {
      b := 0
      return
    }

    if (side == FromLeft && trans == NoTranspose) {
      // B := alpha A^(-1) B
      if(uplo == Upper) {
        cforRange(0 until n) { j =>
          if(alpha != 1.0) cforRange(0 until m) { i => b(i,j) *= alpha }
          cforRange(m-1 to 0 by -1) { k =>
            if(b(k,j) != 0) {
              if(diag == NonUnitDiagonal) b(k,j) /= a(k,k)
              cforRange(0 to k-1) { i => b(i,j) -= b(k,j)*a(i,k) }
            }
          }
        }
      }
      else {
        cforRange(0 until n) { j =>
          if(alpha != 1.0) cforRange(0 until m) { i => b(i,j) *= alpha }
          cforRange(0 until m) { k =>
            if(b(k,j) != 0) {
              if(diag == NonUnitDiagonal) b(k,j) /= a(k,k)
              cforRange(k+1 until m) { i => b(i,j) -= b(k,j)*a(i,k) }
            }
          }
        }
      }
    }
    else if (side == FromLeft && trans == Transpose) {
      // B := alpha A^(-T) B
      if(uplo == Upper) {
        cforRange(0 until n) { j =>
          cforRange(0 until m) { i =>
            var t = alpha*b(i,j)
            cforRange(0 to i-1) { k => t -= a(k,i)*b(k,j) }
            if(diag == NonUnitDiagonal) t /= a(i,i)
            b(i,j) = t
          }
        }
      }
      else {
        cforRange(0 until n) { j =>
          cforRange(m-1 to 0 by -1) { i =>
            var t = alpha*b(i,j)
            cforRange(i+1 until m) { k => t -= a(k,i)*b(k,j) }
            if(diag == NonUnitDiagonal) t /= a(i,i)
            b(i,j) = t
          }
        }
      }
    }
    else if (side == FromRight && trans ==  NoTranspose) {
      // B: = alpha B A^(-1)
      if(uplo == Upper) {
        cforRange(0 until n) { j =>
          if(alpha != 1.0) cforRange(0 until m) { i => b(i,j) *= alpha }
          cforRange(0 to j-1) { k =>
            if(a(k,j) != 0)
              cforRange(0 until m) { i => b(i,j) -= a(k,j)*b(i,k) }
          }
          if(diag == NonUnitDiagonal) {
            val t = 1/a(j,j)
            cforRange(0 until m) { i => b(i,j) *= t }
          }
        }
      }
      else {
        cforRange(n-1 to 0 by -1) { j =>
          if(alpha != 1.0) cforRange(0 until m) { i => b(i,j) *= alpha }
          cforRange(j+1 until n) { k =>
            if(a(k,j) != 0)
              cforRange(0 until m) { i => b(i,j) -= a(k,j)*b(i,k) }
          }
          if(diag == NonUnitDiagonal) {
            val t = 1/a(j,j)
            cforRange(0 until m) { i => b(i,j) *= t }
          }
        }
      }
    }
    else if (side == FromRight && trans ==  Transpose) {
      // B: = alpha B A^(-T)
      if(uplo == Upper) {
        cforRange(n-1 to 0 by -1) { k =>
          if(diag == NonUnitDiagonal) {
            val t = 1.0/a(k,k)
            cforRange(0 until m) { i => b(i,k) *= t }
          }
          cforRange(0 to k-1) { j =>
            if(a(j,k) != 0) {
              val t = a(j,k)
              cforRange(0 until m) { i => b(i,j) -= t*b(i,k) }
            }
          }
          if(alpha != 0) cforRange(0 until m) { i => b(i,k) *= alpha }
        }
      }
      else {
        cforRange(0 until n) { k =>
          if(diag == NonUnitDiagonal) {
            val t = 1.0/a(k,k)
            cforRange(0 until m) { i => b(i,k) *= t }
          }
          cforRange(k+1 until n) { j =>
            if(a(j,k) != 0) {
              val t = a(j,k)
              cforRange(0 until m) { i => b(i,j) -= t*b(i,k) }
            }
          }
          if(alpha != 0) cforRange(0 until m) { i => b(i,k) *= alpha }
        }
      }
    }
  }
}

/** Convenience object to enable `import` instead of `extends` */
object NaiveLevel3 extends NaiveLevel3


