package spire.matrix.dense

import scala.math.sqrt

/**
 * Computations of Euclidean norms that safely avoids overflows
 * and precision loss.
 *
 * This is key to the robust computation of Householder reflections,
 * which are central to many algorithms.
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
trait EuclideanNorm {

  /**
   * Euclidean norm of the vector v
   *
   * Reference: subroutine DNRM2 in LAPACK [1]
   */
  def euclideanNorm(v: Iterable[Double]): Double = {
    if(v.hasDefiniteSize) {
      if(v.size == 0) return 0.0
      else if(v.size == 1) return v.head.abs
    }
    var scale = 0.0
    var sumSquares = 1.0
    for(x <- v) {
      if(x != 0.0) {
        val xa = x.abs
        if(scale < xa) {
          val r = scale/xa
          sumSquares = 1.0 + sumSquares*r*r
          scale = xa
        }
        else {
          val r = xa/scale
          sumSquares += r*r
        }
      }
    }
    scale*sqrt(sumSquares)
  }

  /**
   * Euclidean norm of (x,y)
   *
   * Reference: subroutine DLAPY2 in LAPACK [1]
   */
  def euclideanNorm2D(x:Double, y:Double) = {
    val (xa, ya) = (x.abs, y.abs)
    val (min, max) = if(xa < ya) (xa, ya) else (ya, xa)
    if(min == 0) {
      max
    }
    else {
      val r = min/max
      max*sqrt(1.0 + r*r)
    }
  }
}
