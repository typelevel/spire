package spire.benchmark

import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.math.Implicits._
import fpf._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object ComplexAddBenchmarks extends MyRunner { val cls = classOf[ComplexAddBenchmarks] }
class ComplexAddBenchmarks extends MyBenchmark {
  @Param(Array("500000", "1000000", "2000000", "4000000", "8000000", "16000000"))
  var size:Int = 0

  var complexes:Array[Complex[Double]] = null
  var fcomplexes:Array[Long] = null

  override protected def setUp() {
    complexes = init(size)(Complex(nextDouble(), nextDouble()))
    fcomplexes = init(size)(FastComplex(nextFloat(), nextFloat()))
  }

  def addGeneric[@spec(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
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

  def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
  def timeAddFastComplexes(reps:Int) = run(reps)(addFastComplexes(fcomplexes))
}
