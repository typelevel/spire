package spire
package benchmark.jmh

import java.util.concurrent.TimeUnit


import org.openjdk.jmh.annotations._

import spire.algebra._
import spire.implicits._
import spire.math._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class AddBenchmarks {

  def addGeneric[@sp(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addFastComplexes(data:Array[Long]):Long = {
    var total = FastComplex(0.0F, 0.0F)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def addIntsDirect(state:IntState): Int = {
    val data = state.values
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  @Benchmark
  def addIntsGeneric(state:IntState): Int = addGeneric(state.values)

  @Benchmark
  def addLongsDirect(state:LongState): Long = {
    val data = state.values
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total

  }

  @Benchmark
  def addLongsGeneric(state:LongState): Long = addGeneric(state.values)

  @Benchmark
  def addFloatsDirect(state:FloatState): Float = {
    val data = state.values
    var total = 0.0F
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total

  }

  @Benchmark
  def addFloatsGeneric(state:FloatState): Float = addGeneric(state.values)

  @Benchmark
  def addDoublesDirect(state:DoubleState): Double = {
    val data = state.values
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  @Benchmark
  def addDoublesGeneric(state:DoubleState): Double = addGeneric(state.values)

  @Benchmark
  def addComplexesDirect(state:ComplexState): Complex[Double] = {
    val data = state.values
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  @Benchmark
  def addComplexesGeneric(state:ComplexState): Complex[Double] = addGeneric(state.values)

  @Benchmark
  def addFastComplexes(state:FastComplexState): Long = {
    val data = state.values
    var total = FastComplex(0.0F, 0.0F)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

}
