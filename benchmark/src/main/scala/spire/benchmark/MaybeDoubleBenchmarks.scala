package spire.benchmark

import scala.{specialized => spec}
import scala.util.Random._

import spire.algebra._
import spire.math._
import spire.math.Implicits._
import fpf._

import com.google.caliper.Param

object MaybeDoubleBenchmarks extends MyRunner2(classOf[MaybeDoubleBenchmarks])

class MaybeDoubleBenchmarks extends MyBenchmark2 {
  //@Param(Array("20", "21", "22", "23", "24"))
  @Param(Array("18"))
  var pow:Int = 0

  var maybeDoubles:Array[MaybeDouble] = null
  var maybeFloats:Array[Long] = null

  override protected def setUp() {
    val size = scala.math.pow(2, pow).toInt
    maybeDoubles = init(size)(MaybeDouble(nextDouble))
    maybeFloats = init(size)(FastMaybeFloat(nextFloat))
  }

  def timeAddDirect(reps:Int) = run(reps)(addDirect(maybeDoubles))
  def timeAddGeneric(reps:Int) = run(reps)(addGeneric(maybeDoubles))
  def timeAddFastMaybeFloats(reps:Int) = run(reps)(addFastMaybeFloats(maybeFloats))

  def addGeneric[@spec(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addDirect(data: Array[MaybeDouble]): MaybeDouble = {
    var total = MaybeDouble(0.0)
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastMaybeFloats(data: Array[Long]): Long = {
    var total = FastMaybeFloat(0f)
    var i = 0
    val len = data.length - 1

    // This is slightly different from the others, because it'll overflow the
    // FastMaybeFloat and apparently adding NaNs and Infinities is quite a bit
    // slower than regular fp ops.
    
    while (i < len) { total += FastMaybeFloat.plus(data(i), data(i + 1)); i += 1 }
    total
  }
}
