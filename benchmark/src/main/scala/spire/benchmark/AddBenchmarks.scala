package spire.benchmark

import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._
import fpf._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math
import java.math.BigInteger

object AddBenchmarks extends MyRunner(classOf[AddBenchmarks])

class AddBenchmarks extends MyBenchmark with BenchmarkData {
  def addGeneric[@spec(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addIntsDirect(data:Array[Int]):Int = {
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addLongsDirect(data:Array[Long]):Long = {
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFloatsDirect(data:Array[Float]):Float = {
    var total = 0.0F
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addDoublesDirect(data:Array[Double]):Double = {
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addMaybeDoublesDirect(data: Array[MaybeDouble]): MaybeDouble = {
    var total = MaybeDouble(0.0)
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastMaybeFloatsDirect(data: Array[Long]): Long = {
    var total = FastMaybeFloat(0f)
    var i = 0
    val len = data.length - 1

    // This is slightly different from the others, because it'll overflow the
    // FastMaybeFloat and apparently adding NaNs and Infinities is quite a bit
    // slower than regular fp ops.
    
    while (i < len) { total += FastMaybeFloat.plus(data(i), data(i + 1)); i += 1 }
    total
  }

  def addComplexesDirect(data:Array[Complex[Double]]):Complex[Double] = {
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastComplexes(data:Array[Long]):Long = {
    var total = FastComplex(0.0F, 0.0F)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  def timeAddIntsDirect(reps:Int) = run(reps)(addIntsDirect(ints))
  def timeAddIntsGeneric(reps:Int) = run(reps)(addGeneric(ints))
  
  def timeAddLongsDirect(reps:Int) = run(reps)(addLongsDirect(longs))
  def timeAddLongsGeneric(reps:Int) = run(reps)(addGeneric(longs))
  
  def timeAddFloatsDirect(reps:Int) = run(reps)(addFloatsDirect(floats))
  def timeAddFloatsGeneric(reps:Int) = run(reps)(addGeneric(floats))
  def timeAddFastMaybeFloatsDirect(reps:Int) = run(reps)(addFastMaybeFloatsDirect(maybeFloats))
  
  def timeAddDoublesDirect(reps:Int) = run(reps)(addDoublesDirect(doubles))
  def timeAddDoublesGeneric(reps:Int) = run(reps)(addGeneric(doubles))
  def timeAddMaybeDoublesDirect(reps:Int) = run(reps)(addMaybeDoublesDirect(maybeDoubles))

  def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
  def timeAddFastComplexes(reps:Int) = run(reps)(addFastComplexes(fcomplexes))
}
