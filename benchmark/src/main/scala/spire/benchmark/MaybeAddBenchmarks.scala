package spire.benchmark

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

object MaybeAddBenchmarks extends MyRunner(classOf[MaybeAddBenchmarks])

class MaybeAddBenchmarks extends MyBenchmark {
  @Param(Array("2000000", "4000000", "8000000"))
  var size:Int = 0

  var maybeDoubles:Array[MaybeDouble] = null
  var maybeFloats:Array[Long] = null

  override protected def setUp() {
    maybeDoubles = init(size)(MaybeDouble(nextDouble))
    maybeFloats = init(size)(FastMaybeFloat(nextFloat))
  }

  def addMaybeDoublesDirect(data: Array[MaybeDouble]): MaybeDouble = {
    var total = MaybeDouble(0.0)
    var i = 0
    val len = data.length - 1
    while (i < len) { total = data(i) + data(i + 1); i += 1 }
    total
  }

  def addFastMaybeFloatsDirect(data: Array[Long]): Long = {
    var total = FastMaybeFloat(0f)
    var i = 0
    val len = data.length - 1
    while (i < len) { total = FastMaybeFloat.plus(data(i), data(i + 1)); i += 1 }
    total
  }

  def timeAddFastMaybeFloatsDirect(reps:Int) = run(reps)(addFastMaybeFloatsDirect(maybeFloats))
  def timeAddMaybeDoublesDirect(reps:Int) = run(reps)(addMaybeDoublesDirect(maybeDoubles))
}
