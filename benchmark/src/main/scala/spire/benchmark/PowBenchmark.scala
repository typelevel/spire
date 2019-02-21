package spire
package benchmark
/*

import scala.util.Random
import Random._

import spire.implicits._

object PowBenchmarks extends MyRunner(classOf[PowBenchmarks])

class PowBenchmarks extends MyBenchmark {

  var longs: Array[Long] = null
  var ints: Array[Int] = null

  override def setUp(): Unit = {
    ints = init(200000)(nextInt)
    longs = init(200000)(nextLong)
  }

  def timeLongPowForInt(reps:Int) = run(reps) {
    var t = 0
    ints.foreach { n =>
      t += spire.math.pow(n.toLong, 2.toLong).toInt
    }
    t
  }

  def timeDoublePowForInt(reps:Int) = run(reps) {
    var t = 0
    ints.foreach { n =>
      t += spire.math.pow(n.toDouble, 2.0).toInt
    }
    t
  }

  def timeBigIntPowForInt(reps:Int) = run(reps) {
    var t = 0
    ints.foreach { n =>
      t += (BigInt(n) pow 2).toInt
    }
    t
  }

  def timeLongPowForLong(reps:Int) = run(reps) {
    var t = 0L
    longs.foreach { n =>
      t += spire.math.pow(n, 2L)
    }
    t
  }

  def timeDoublePowForLong(reps:Int) = run(reps) {
    var t = 0L
    longs.foreach { n =>
      t += spire.math.pow(n.toDouble, 2.0).toLong
    }
    t
  }

  def timeBigIntPowForLong(reps:Int) = run(reps) {
    var t = 0L
    longs.foreach { n =>
      t += (BigInt(n) pow 2).toLong
    }
    t
  }

  def timeDoublePowForDouble(reps:Int) = run(reps) {
    var t = 0.0
    longs.foreach { n =>
      t += spire.math.pow(n, 2.0)
    }
    t
  }
}
*/