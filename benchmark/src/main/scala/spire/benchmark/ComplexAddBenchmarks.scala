package spire
package benchmark.jmh
import java.util.concurrent.TimeUnit

import scala.util.Random
import Random._
import org.openjdk.jmh.annotations._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.benchmark.Arrays.init

/*
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class ComplexAddBenchmarks {
  @Param(Array("500000", "1000000", "2000000", "4000000", "8000000", "16000000"))
  var size:Int = 0

  var complexes:Array[Complex[Float]] = null
  var longs:Array[Long] = null
  var fcs:Array[FloatComplex] = null

  @Setup
  def setup(): Unit = {
    complexes = init(size)(Complex(nextFloat(), nextFloat()))
    longs = init(size)(FastComplex(nextFloat(), nextFloat()))
    fcs = init(size)(FloatComplex(nextFloat(), nextFloat()))
  }

  def addGeneric[@sp(Float) A:Ring](data: Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addComplexesDirect(data: Array[Complex[Float]]):Complex[Float] = {
    var total = Complex.zero[Float]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastComplexes(data: Array[Long]):Long = {
    var total = FastComplex(0.0F, 0.0F)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  def addFloatComplexesBoxed(data: Array[FloatComplex]):FloatComplex = {
    var total = FloatComplex(0.0F, 0.0F)
    var i = 0
    val len = fcs.length
    while (i < len) { total += fcs(i); i += 1 }
    total
  }

  def addFloatComplexesUnboxed(data: Array[Long]):FloatComplex = {
    var total = FloatComplex(0.0F, 0.0F)
    var i = 0
    val len = fcs.length
    while (i < len) { total += new FloatComplex(longs(i)); i += 1 }
    total
  }

  def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
  def timeAddFastComplexes(reps:Int) = run(reps)(addFastComplexes(longs))
  def timeAddFloatComplexesBoxed(reps:Int) = run(reps)(addFloatComplexesBoxed(fcs))
  def timeAddFloatComplexesUnboxed(reps:Int) = run(reps)(addFloatComplexesUnboxed(longs))
}
*/