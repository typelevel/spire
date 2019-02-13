package spire
package benchmark
/*
import scala.util.Random
import Random._

import spire.algebra._
import spire.implicits._

import com.google.caliper.Param

object AnyValAddBenchmarks extends MyRunner(classOf[AnyValAddBenchmarks])

class AnyValAddBenchmarks extends MyBenchmark {
  //@Param(Array("1000000", "2000000", "4000000", "8000000", "16000000"))
  @Param(Array("1000000", "2000000", "4000000"))
  var size:Int = 0

  var bytes:Array[Byte] = null
  var shorts:Array[Short] = null
  var ints:Array[Int] = null
  var longs:Array[Long] = null
  var floats:Array[Float] = null
  var doubles:Array[Double] = null

  override protected def setUp(): Unit = {
    bytes = init(size)(nextInt.toByte)
    shorts = init(size)(nextInt.toShort)
    ints = init(size)(nextInt)
    longs = init(size)(nextLong)
    floats = init(size)(nextFloat)
    doubles = init(size)(nextDouble)
  }

  def addGeneric[@sp(Byte, Short, Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addBytesDirect(data:Array[Byte]):Int = {
    var total = 0.toByte
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) + total).toByte; i += 1 }
    total
  }

  def addShortsDirect(data:Array[Short]):Int = {
    var total = 0.toShort
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) + total).toShort; i += 1 }
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

  def timeAddBytesDirect(reps:Int) = run(reps)(addBytesDirect(bytes))
  def timeAddBytesGeneric(reps:Int) = run(reps)(addGeneric(bytes))
  def timeAddShortsDirect(reps:Int) = run(reps)(addShortsDirect(shorts))
  def timeAddShortsGeneric(reps:Int) = run(reps)(addGeneric(shorts))
  def timeAddIntsDirect(reps:Int) = run(reps)(addIntsDirect(ints))
  def timeAddIntsGeneric(reps:Int) = run(reps)(addGeneric(ints))
  def timeAddLongsDirect(reps:Int) = run(reps)(addLongsDirect(longs))
  def timeAddLongsGeneric(reps:Int) = run(reps)(addGeneric(longs))
  def timeAddFloatsDirect(reps:Int) = run(reps)(addFloatsDirect(floats))
  def timeAddFloatsGeneric(reps:Int) = run(reps)(addGeneric(floats))
  def timeAddDoublesDirect(reps:Int) = run(reps)(addDoublesDirect(doubles))
  def timeAddDoublesGeneric(reps:Int) = run(reps)(addGeneric(doubles))
}
*/