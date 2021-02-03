package spire
package optional

import java.lang.Math

import spire.algebra.Order

/**
 * This provides orderings (Order and Eq) for Float and Double that have
 * a total order. Specifically, this will order NaN's consistently, rather
 * than having their order be undefined. However, this won't be as fast as
 * the default ordering.
 */
object totalfloat {
  trait TotalFloatOrder extends Order[Float] {
    override def eqv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) == 0
    override def neqv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) != 0
    override def gt(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) > 0
    override def gteqv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) >= 0
    override def lt(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) > 0
    override def lteqv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) >= 0
    override def min(x: Float, y: Float): Float = if (java.lang.Float.compare(x, y) < 0) x else y
    override def max(x: Float, y: Float): Float = Math.max(x, y)
    def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
  }
  implicit final val TotalFloatOrder = new TotalFloatOrder {}

  trait TotalDoubleOrder extends Order[Double] {
    override def eqv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) == 0
    override def neqv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) != 0
    override def gt(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) > 0
    override def gteqv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) >= 0
    override def lt(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) > 0
    override def lteqv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) >= 0
    override def min(x: Double, y: Double): Double = if (java.lang.Double.compare(x, y) < 0) x else y
    override def max(x: Double, y: Double): Double = Math.max(x, y)
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  }
  implicit final val TotalDoubleOrder = new TotalDoubleOrder {}
}
