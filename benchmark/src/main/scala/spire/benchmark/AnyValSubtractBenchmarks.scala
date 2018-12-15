package spire
package benchmark
/*
import scala.util.Random
import Random._

import spire.algebra._
import spire.implicits._

import com.google.caliper.Param

object AnyValSubtractBenchmarks extends MyRunner(cls = classOf[AnyValSubtractBenchmarks])

class AnyValSubtractBenchmarks extends MyBenchmark {
  @Param(Array("1000000", "2000000", "4000000", "8000000", "16000000"))
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

  def subtractGeneric[@sp(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].minus(data(i), total); i += 1 }
    total
  }

  def subtractBytesDirect(data:Array[Byte]):Int = {
    var total = 0.toByte
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total).toByte; i += 1 }
    total
  }

  def subtractShortsDirect(data:Array[Short]):Int = {
    var total = 0.toShort
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total).toShort; i += 1 }
    total
  }

  def subtractIntsDirect(data:Array[Int]):Int = {
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total); i += 1 }
    total
  }

  def subtractLongsDirect(data:Array[Long]):Long = {
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total); i += 1 }
    total
  }

  def subtractFloatsDirect(data:Array[Float]):Float = {
    var total = 0.0F
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total); i += 1 }
    total
  }

  def subtractDoublesDirect(data:Array[Double]):Double = {
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total = (data(i) - total); i += 1 }
    total
  }

  def timeSubtractBytesDirect(reps:Int) = run(reps)(subtractBytesDirect(bytes))
  def timeSubtractBytesGeneric(reps:Int) = run(reps)(subtractGeneric(bytes))
  def timeSubtractShortsDirect(reps:Int) = run(reps)(subtractShortsDirect(shorts))
  def timeSubtractShortsGeneric(reps:Int) = run(reps)(subtractGeneric(shorts))
  def timeSubtractIntsDirect(reps:Int) = run(reps)(subtractIntsDirect(ints))
  def timeSubtractIntsGeneric(reps:Int) = run(reps)(subtractGeneric(ints))
  def timeSubtractLongsDirect(reps:Int) = run(reps)(subtractLongsDirect(longs))
  def timeSubtractLongsGeneric(reps:Int) = run(reps)(subtractGeneric(longs))
  def timeSubtractFloatsDirect(reps:Int) = run(reps)(subtractFloatsDirect(floats))
  def timeSubtractFloatsGeneric(reps:Int) = run(reps)(subtractGeneric(floats))
  def timeSubtractDoublesDirect(reps:Int) = run(reps)(subtractDoublesDirect(doubles))
  def timeSubtractDoublesGeneric(reps:Int) = run(reps)(subtractGeneric(doubles))
}
*/