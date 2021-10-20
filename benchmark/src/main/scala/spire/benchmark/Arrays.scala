/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire.benchmark

import org.openjdk.jmh.annotations.{Scope, Setup, State}
import spire.ClassTag
import spire.algebra.Order
import spire.math.{Complex, FastComplex, FloatComplex}

import scala.util.Random._

sealed trait ArrayOrder

object ArrayOrder {
  case object Random extends ArrayOrder
  case object Sorted extends ArrayOrder
  case object Reversed extends ArrayOrder
}

object Arrays {

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def init[A: ClassTag](size: Int)(f: => A): Array[A] = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = f
    data
  }

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def mkarray[A: ClassTag: Order](size: Int, layout: ArrayOrder)(f: => A): Array[A] = {
    val data = init(size)(f)
    val ct = implicitly[ClassTag[A]]
    val order = Order[A]
    layout match {
      case ArrayOrder.Random   =>
      case ArrayOrder.Sorted   => spire.math.Sorting.sort(data)(order, ct)
      case ArrayOrder.Reversed => spire.math.Sorting.sort(data)(Order.reverse(order), ct)
      case _                   => sys.error(s"unknown layout: $layout")
    }
    data
  }

  def nextComplexDouble(): Complex[Double] = Complex(nextDouble(), nextDouble())
  def nextComplexFloat(): Complex[Float] = Complex(nextFloat(), nextFloat())
  def nextFastComplex(): Long = FastComplex(nextFloat(), nextFloat())
  def nextFloatComplex(): FloatComplex = FloatComplex(nextFloat(), nextFloat())

  val size = 200 * 1000

  @State(Scope.Thread)
  class IntState {
    var values: Array[Int] = _
    @Setup
    def setup(): Unit = values = init(size)(nextInt())
  }

  @State(Scope.Thread)
  class LongState {
    var values: Array[Long] = _
    @Setup
    def setup(): Unit = values = init(size)(nextLong())
  }

  @State(Scope.Thread)
  class FloatState {
    var values: Array[Float] = _
    @Setup
    def setup(): Unit = values = init(size)(nextFloat())
  }

  @State(Scope.Thread)
  class DoubleState {
    var values: Array[Double] = _
    @Setup
    def setup(): Unit = values = init(size)(nextDouble())
  }

  @State(Scope.Thread)
  class FastComplexState {
    var values: Array[Long] = _
    @Setup
    def setup(): Unit = values = init(size)(FastComplex(nextFloat(), nextFloat()))
  }

  @State(Scope.Thread)
  class FloatComplexState {
    var values: Array[FloatComplex] = _
    @Setup
    def setup(): Unit = values = init(size)(nextFloatComplex())
  }

  @State(Scope.Thread)
  class ComplexFloatState {
    var values: Array[Complex[Float]] = _
    @Setup
    def setup(): Unit = values = init(size)(nextComplexFloat())
  }

  @State(Scope.Thread)
  class ComplexDoubleState {
    var values: Array[Complex[Double]] = _
    @Setup
    def setup(): Unit = values = init(size)(nextComplexDouble())
  }

}
