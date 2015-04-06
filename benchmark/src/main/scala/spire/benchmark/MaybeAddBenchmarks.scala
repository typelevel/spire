package spire.benchmark

import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.math.algebraic._
import spire.implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object MaybeAddBenchmarks extends MyRunner(classOf[MaybeAddBenchmarks])

class MaybeAddBenchmarks extends MyBenchmark {
  @Param(Array("2000000", "4000000", "8000000"))
  var size:Int = 0

  var maybeDoubles:Array[MaybeDouble] = null

  override protected def setUp(): Unit = {
    maybeDoubles = init(size)(MaybeDouble(nextDouble))
  }

  def addMaybeDoublesDirect(data: Array[MaybeDouble]): MaybeDouble = {
    var total = MaybeDouble(0.0)
    var i = 0
    val len = data.length - 1
    while (i < len) { total = data(i) + data(i + 1); i += 1 }
    total
  }

  def timeAddMaybeDoublesDirect(reps:Int) = run(reps)(addMaybeDoublesDirect(maybeDoubles))
}
