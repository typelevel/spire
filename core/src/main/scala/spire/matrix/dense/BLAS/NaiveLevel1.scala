package spire.matrix.dense.BLAS

import spire.matrix.dense.{VectorLike, PlaneRotation}

import spire.syntax.cfor._
import scala.collection.mutable.IndexedSeq

import scala.math.abs

/** Reference implementation using textbook loops */
trait NaiveLevel1 extends Level1 {
  def scale(alpha: Double, x: IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => x(k) *= alpha }

  def copy(x: IndexedSeq[Double], y: IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => y(k) = x(k) }

  def axpy(alpha: Double,
           x: IndexedSeq[Double], y: IndexedSeq[Double]): Unit =
    cforRange(0 until x.length) { k => y(k) += alpha * x(k) }

  def rot(x:VectorLike, y:VectorLike, g:PlaneRotation) {
    cforRange(0 until x.length) { i =>
      val xi = g.cs*x(i) + g.sn*y(i)
      y(i) = g.cs*y(i) - g.sn*x(i)
      x(i) = xi
    }
  }

  def dot(x:IndexedSeq[Double], y:IndexedSeq[Double]): Double = {
    var s = 0.0
    cforRange(0 until x.length) { i => s += x(i)*y(i) }
    s
  }

  def idamax(x:IndexedSeq[Double]): Int = {
    val n = x.length
    if(n == 0 || n == 1) 0
    else {
      var amax = abs(x(0))
      var idx = 0
      cforRange(1 until n) { i =>
        val amaxNew = abs(x(i))
        if(amaxNew > amax) {
          amax = amaxNew
          idx = i
        }
      }
      idx
    }
  }

}
