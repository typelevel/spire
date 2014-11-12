package spire.matrix.dense.BLAS

import spire.matrix.dense.{Matrix, Vector, PlaneRotation}

import spire.syntax.cfor._

import scala.math.abs

/** Reference implementation using textbook loops */
trait NaiveLevel1 extends Level1 {
  def scale(alpha: Double, x: Vector): Unit =
    cforRange(0 until x.dimension) { k => x(k) *= alpha }

  def scale(alpha: Double, x: Matrix): Unit = {
    val (m,n) = x.dimensions
    cforRange2(0 until n, 0 until m) { (j,i) => x(i,j) *= alpha }
  }

  def copy(x: Vector, y: Vector): Unit =
    cforRange(0 until x.dimension) { k => y(k) = x(k) }

  def axpy(alpha: Double, x: Vector, y: Vector): Unit =
    cforRange(0 until x.dimension) { k => y(k) += alpha * x(k) }

  def axpy(alpha: Double, x: Matrix, y: Matrix): Unit = {
    val (m,n) = y.dimensions
    cforRange2(0 until n, 0 until m) { (j,i) => y(i,j) += alpha * x(i,j) }
  }

  def rot(x:Vector, y:Vector, g:PlaneRotation) {
    cforRange(0 until x.dimension) { i =>
      val xi = g.cs*x(i) + g.sn*y(i)
      y(i) = g.cs*y(i) - g.sn*x(i)
      x(i) = xi
    }
  }

  def dot(x:Vector, y:Vector): Double = {
    var s = 0.0
    cforRange(0 until x.dimension) { i => s += x(i)*y(i) }
    s
  }

  def idamax(x:Vector): Int = {
    val n = x.dimension
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

/** Convenience object to enable `import` instead of `extends` */
object NaiveLevel1 extends NaiveLevel1
