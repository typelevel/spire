package spire.matrix.dense

import spire.matrix.BLAS
import BLAS.level1
import scala.collection.mutable

trait MagnitudeLimitation
extends mutable.IndexedSeq[Double] with BLAS.level1.Naive {

  /** Rescale this so that the maximum absolute value of any element is amax */
  def rescaleElementMagnitudeTo(amax:Double): Unit = {
    val maxAbs = this.iterator.map(_.abs).max
    assert(!maxAbs.isNaN)
    val alpha = if(maxAbs > 0) amax/maxAbs else 0
    scale(alpha, this)
  }
}
