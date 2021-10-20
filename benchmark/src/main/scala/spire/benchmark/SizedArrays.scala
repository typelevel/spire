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

import org.openjdk.jmh.annotations.{Param, Scope, Setup, State}
import spire.ClassTag
import spire.algebra.Order
import spire.benchmark.Arrays.{nextComplexDouble, nextComplexFloat}
import spire.math.{Complex, FastComplex}

import scala.util.Random.{nextDouble, nextFloat, nextInt, nextLong}

object SizedArrays {

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

  @State(Scope.Thread)
  class IntState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Int] = _
    @Setup
    def setup(): Unit = values = init(size)(nextInt())
  }

  @State(Scope.Thread)
  class LongState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Long] = _
    @Setup
    def setup(): Unit = values = init(size)(nextLong())
  }

  @State(Scope.Thread)
  class FloatState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Float] = _
    @Setup
    def setup(): Unit = values = init(size)(nextFloat())
  }

  @State(Scope.Thread)
  class DoubleState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Double] = _
    @Setup
    def setup(): Unit = values = init(size)(nextDouble())
  }

  @State(Scope.Thread)
  class FastComplexState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Long] = _
    @Setup
    def setup(): Unit = values = init(size)(FastComplex(nextFloat(), nextFloat()))
  }

  @State(Scope.Thread)
  class ComplexFloatState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Complex[Float]] = _
    @Setup
    def setup(): Unit = values = init(size)(nextComplexFloat())
  }

  @State(Scope.Thread)
  class ComplexDoubleState {
    @Param(Array("100000", "200000", "400000"))
    var size: Int = 0
    var values: Array[Complex[Double]] = _
    @Setup
    def setup(): Unit = values = init(size)(nextComplexDouble())
  }

}
