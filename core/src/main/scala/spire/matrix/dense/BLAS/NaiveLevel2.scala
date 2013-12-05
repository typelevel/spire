package spire.matrix.dense.BLAS

import spire.syntax.cfor._

import spire.matrix.dense.MatrixLike
import spire.matrix.dense.VectorLike
import spire.matrix.{Transposition,UpperOrLower,DiagonalProperty}
import Transposition._
import UpperOrLower._
import DiagonalProperty._

/**
 * Straightforward but inefficient implementations
 *
 * They are to be considered as a reference implementations.
 */
trait NaiveLevel2 extends Level2 {

   def gemv(trans: Transposition.Value,
            alpha: Double, a: MatrixLike,
            x: VectorLike, beta: Double, y: VectorLike): Unit = {
    if (trans == NoTranspose)
      require(y.length == a.dimensions._1 && a.dimensions._2 == x.length)
    else
      require(y.length == a.dimensions._2 && a.dimensions._1 == x.length)

    // y := beta y
    if(beta == 0)
      cforRange(0 until y.length) { i => y(i) = 0 }
    else if(beta != 1)
      cforRange(0 until y.length) { i => y(i) *= beta }

    // y += alpha op(A) x
    if(alpha != 0) {
      val (m, n) = a.dimensions
      if(trans == NoTranspose) {
        cforRange(0 until n) { j =>
          if (x(j) != 0) {
            val t = alpha * x(j)
            cforRange(0 until m) { i => y(i) += t * a(i,j) }
          }
        }
      } else {
        cforRange(0 until n) { j =>
          var t = 0.0
          cforRange(0 until m) { i => t += a(i, j) * x(i) }
          y(j) += alpha * t
        }
      }
    }
   }

  def ger(alpha: Double, x: VectorLike, y: VectorLike, a: MatrixLike): Unit = {
    require((x.length, y.length) == a.dimensions)

    cforRange(0 until y.length) { j =>
      if (y(j) != 0) {
        val t = alpha * y(j)
        cforRange(0 until x.length) { i => a(i, j) += x(i) * t }
      }
    }
  }

  def trmv(uplo:UpperOrLower.Value, trans:Transposition.Value,
           diag:DiagonalProperty.Value, a:MatrixLike, x:VectorLike) {
    if(trans == NoTranspose) require(a.dimensions._2 == x.length)
    else                     require(a.dimensions._1 == x.length)

    val n = x.length
    if(n == 0) return

    if(trans == NoTranspose) {
      if(uplo == Upper) {
        cforRange(0 until n) { j =>
          if(x(j) != 0) {
            val x_j = x(j)
            cforRange(0 to j-1) { i => x(i) += a(i,j)*x_j }
            if(diag == NonUnitDiagonal) x(j) *= a(j,j)
          }
        }
      }
      else if(uplo == Lower) {
        cforRange(n-1 to 0 by -1) { j =>
          if(x(j) != 0) {
            val x_j = x(j)
            cforRange(n-1 to j+1 by -1) { i => x(i) += a(i,j)*x_j }
            if(diag == NonUnitDiagonal) x(j) *= a(j,j)
          }

        }
      }
    }
    else if(trans == Transpose) {
      if(uplo == Upper) {
        cforRange(n-1 to 0 by -1) { j =>
          var x_j = x(j)
          if(diag == NonUnitDiagonal) x_j *= a(j,j)
          cforRange(j-1 to 0 by -1) { i => x_j += a(i,j)*x(i) }
          x(j) = x_j
        }
      }
      else if(uplo == Lower) {
        cforRange(0 until n) { j =>
          var x_j = x(j)
          if(diag == NonUnitDiagonal) x_j *= a(j,j)
          cforRange(j+1 until n) { i => x_j += a(i,j)*x(i) }
          x(j) = x_j
        }
      }
    }
  }

}
