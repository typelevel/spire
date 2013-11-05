package spire.matrix.dense.BLAS

import spire.syntax.cfor._

import spire.matrix.dense.MatrixLike
import spire.matrix.dense.VectorLike
import spire.matrix.Transposition
import Transposition._

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
}
