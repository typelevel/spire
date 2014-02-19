package spire.matrix.dense

import scala.collection.mutable

trait MagnitudeLimitation extends Iterable[Double] {

  /** Scale to use to bring the maximum absolute value of any element to amax */
  def magnitudeLimitingScale(amax:Double) = {
    val maxAbs = this.iterator.map(_.abs).max
    assert(!maxAbs.isNaN)
    if(maxAbs > 0) amax/maxAbs else 0
  }
}

trait MatrixMagnitudeLimitation
extends Matrix with MagnitudeLimitation with BLAS.NaiveLevel1 {
  def rescaleElementMagnitudeTo(amax:Double) {
    scale(magnitudeLimitingScale(amax), this)
  }
}

trait VectorMagnitudeLimitation
extends Vector with MagnitudeLimitation with BLAS.NaiveLevel1 {
  def rescaleElementMagnitudeTo(amax:Double) {
    scale(magnitudeLimitingScale(amax), this)
  }
}

