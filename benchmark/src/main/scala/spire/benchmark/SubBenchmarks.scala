package spire
package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import spire.algebra._
import spire.benchmark.Arrays._
import spire.implicits._
import spire.math._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class SubBenchmarks {

  def subGeneric[@sp(Int, Long, Float, Double) A: Ring](data: Array[A]): A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].minus(total, data(i)); i += 1 }
    total
  }

  def subFastComplex(data: Array[Long]): Long = {
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.subtract(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def subIntDirect(state: IntState): Int = {
    val data = state.values
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total
  }
  @Benchmark
  def subIntGeneric(state: IntState): Int = subGeneric(state.values)

  @Benchmark
  def subLongDirect(state: LongState): Long = {
    val data = state.values
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total
  }
  @Benchmark
  def subLongGeneric(state: LongState): Long = subGeneric(state.values)

  @Benchmark
  def subFloatDirect(state: FloatState): Float = {
    val data = state.values
    var total = 0.0f
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total

  }
  @Benchmark
  def subFloatGeneric(state: FloatState): Float = subGeneric(state.values)

  @Benchmark
  def subDoubleDirect(state: DoubleState): Double = {
    val data = state.values
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total
  }
  @Benchmark
  def subDoublesGeneric(state: DoubleState): Double = subGeneric(state.values)

  @Benchmark
  def subComplexFloatDirect(state: ComplexFloatState): Complex[Float] = {
    val data = state.values
    var total = Complex.zero[Float]
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total
  }
  @Benchmark
  def subComplexFloatGeneric(state: ComplexFloatState): Complex[Float] = subGeneric(state.values)

  @Benchmark
  def subFastComplexDirect(state: FastComplexState): Long = {
    val data = state.values
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.subtract(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def subComplexDoubleDirect(state: ComplexDoubleState): Complex[Double] = {
    val data = state.values
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { total -= data(i); i += 1 }
    total
  }
  @Benchmark
  def subComplexDoubleGeneric(state: ComplexDoubleState): Complex[Double] = subGeneric(state.values)

}
