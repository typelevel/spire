package spire
package benchmark
/*
import com.google.caliper.Param

import java.math.MathContext.UNLIMITED

import scala.util.Random

import spire.math._
import spire.std.bigDecimal._

case class Point2(x: Double, y: Double)
trait Orient2 {
  def orient(p: Point2, q: Point2, r: Point2): Int
}

object Orient2 {
  // Very fast, but may be incorrect.
  val bad = new Orient2 {
    def orient(p: Point2, q: Point2, r: Point2): Int = {
      spire.math.signum((q.x - p.x) * (r.y - p.y) - (r.x - p.x) * (q.y - p.y)).toInt
    }
  }

  // Slow, but correct.
  val slow = new Orient2 {
    def orient(p: Point2, q: Point2, r: Point2): Int = {
      val px = BigDecimal(p.x, UNLIMITED)
      val py = BigDecimal(p.y, UNLIMITED)
      val qx = BigDecimal(q.x, UNLIMITED)
      val qy = BigDecimal(q.y, UNLIMITED)
      val rx = BigDecimal(r.x, UNLIMITED)
      val ry = BigDecimal(r.y, UNLIMITED)
      ((qx - px) * (ry - py) - (rx - px) * (qy - py)).signum
    }
  }

  // Fast and correct.
  val fast = new Orient2 {
    def orient(p: Point2, q: Point2, r: Point2): Int = {
      val px = FpFilter.exact[BigDecimal](p.x)
      val py = FpFilter.exact[BigDecimal](p.y)
      val qx = FpFilter.exact[BigDecimal](q.x)
      val qy = FpFilter.exact[BigDecimal](q.y)
      val rx = FpFilter.exact[BigDecimal](r.x)
      val ry = FpFilter.exact[BigDecimal](r.y)
      ((qx - px) * (ry - py) - (rx - px) * (qy - py)).signum
    }
  }
}

object FpFilterBenchmark extends MyRunner(classOf[FpFilterBenchmark])

class FpFilterBenchmark extends MyBenchmark {
  @Param(Array("32", "128", "1024"))
  var size: Int = 0

  var points: Array[Point2] = _

  override protected def setUp(): Unit = {
    points = init(size)(Point2(Random.nextDouble, Random.nextDouble))
  }

  def findSign(o: Orient2): Int = {
    val ps = points
    var i = 2
    var sign = 0
    while (i < ps.length) {
      sign = sign ^ o.orient(ps(i - 2), ps(i - 1), ps(i))
      i += 1
    }
    sign
  }

  def timeDouble(reps: Int) = run(reps)(findSign(Orient2.bad))
  def timeBigDecimal(reps: Int) = run(reps)(findSign(Orient2.slow))
  def timeFpFilter(reps: Int) = run(reps)(findSign(Orient2.fast))
}
*/