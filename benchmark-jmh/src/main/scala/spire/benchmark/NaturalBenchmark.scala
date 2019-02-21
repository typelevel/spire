package spire
package benchmark
/*

import scala.util.Random

import spire.math._
import spire.implicits._

import com.google.caliper.Param

import java.lang.Math

object NaturalBenchmarks extends MyRunner(classOf[NaturalBenchmarks])

class NaturalBenchmarks extends MyBenchmark {
  import spire.math.SafeLong.SafeLongAlgebra

  //@Param(Array("8", "16", "32", "64", "96", "128", "192", "256"))
  //@Param(Array("8"))
  @Param(Array("8", "16", "32", "64", "128"))
  var bits: Int = 0

  //@Param(Array("10", "15", "20"))
  @Param(Array("10"))
  var pow: Int = 0

  var size: Int = 0

  var nats: Array[Natural] = _
  var bigints: Array[BigInt] = _
  var safes: Array[SafeLong] = _

  override def setUp(): Unit = {
    size = Math.pow(2, pow).toInt
    bigints = init(size)(BigInt(bits, Random))
    nats = bigints.map(Natural(_))
    safes = bigints.map(SafeLong(_))
  }

  def timeNaturalSum(reps: Int) = run(reps)(nats.qsum)
  def timeBigIntSum(reps: Int) = run(reps)(bigints.qsum)
  def timeSafeLongSums(reps: Int) = run(reps)(safes.qsum)

  def timeNaturalSumDoubles(reps: Int) = run(reps)(nats.map(n => n << 1).qsum)
  def timeBigIntSumDoubles(reps: Int) = run(reps)(bigints.map(n => n << 1).qsum)
  def timeSafeLongSumDoubles(reps: Int) = run(reps)(safes.map(n => n * 2).qsum)

  def timeNaturalSumSquares(reps: Int) = run(reps)(nats.map(n => n * n).qsum)
  def timeBigIntSumSquares(reps: Int) = run(reps)(bigints.map(n => n * n).qsum)
  def timeSafeLongSumSquares(reps: Int) = run(reps)(safes.map(n => n * n).qsum)

  def timeNaturalSumNormalized(reps: Int) = run(reps)(nats.map(n => n / UInt(10)).qsum)
  def timeBigIntSumNormalized(reps: Int) = run(reps)(bigints.map(n => n / 10).qsum)
  def timeSafeLongSumNormalized(reps: Int) = run(reps)(safes.map(n => n / 10).qsum)

  def timeNaturalMin(reps: Int) = run(reps)(nats.qmin)
  def timeBigIntMin(reps: Int) = run(reps)(bigints.qmin)
  def timeSafeLongMin(reps: Int) = run(reps)(safes.qmin)
}
*/