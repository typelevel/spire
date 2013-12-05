package spire.matrix.dense

import scala.collection.mutable

trait MagnitudeLimitation
extends Matrix with BLAS.NaiveLevel1 {

  /** Rescale this so that the maximum absolute value of any element is amax */
  def rescaleElementMagnitudeTo(amax:Double): Unit = {
    val maxAbs = this.iterator.map(_.abs).max
    assert(!maxAbs.isNaN)
    val alpha = if(maxAbs > 0) amax/maxAbs else 0
    scale(alpha, this)
  }
}
