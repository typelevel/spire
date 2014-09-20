package spire.benchmark

import spire.math.Complex

import scala.reflect.ClassTag

import spire.algebra.Order

import scala.util.Random._

trait FixtureSupport {

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def init[A:ClassTag](size:Int)(f: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = f
    data
  }

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def mkarray[A:ClassTag:Order](size:Int, layout:String)(f: => A): Array[A] = {
    val data = init(size)(f)
    val ct = implicitly[ClassTag[A]]
    val order = Order[A]
    layout match {
      case "random" =>
      case "sorted" => spire.math.Sorting.sort(data)(order, ct)
      case "reversed" => spire.math.Sorting.sort(data)(order.reverse, ct)
      case _ => sys.error(s"unknown layout: $layout")
    }
    data
  }

  def nextComplex = Complex(nextDouble, nextDouble)

}
