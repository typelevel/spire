package spire
package benchmark
/*
import scala.util.Random
import Random._

import spire.math._
import spire.implicits._

import com.google.caliper.Param

import org.apfloat._

import org.jscience.mathematics.number

object RatComparisonBenchmarks extends MyRunner(classOf[RatComparisonBenchmarks])

class RatComparisonBenchmarks extends MyBenchmark {
  @Param(Array("100"))
  var size:Int = 0

  var spires:Array[Rational] = null
  var aps:Array[Aprational] = null
  var jscis:Array[number.Rational] = null

  var nums:Array[Int] = null
  var denoms:Array[Int] = null

  override protected def setUp(): Unit = {
    nums = init(size)(nextInt)
    denoms = init(size)(nextInt)

    spires = new Array[Rational](size)
    aps = new Array[Aprational](size)
    jscis = new Array[number.Rational](size)
    for (i <- 0 until size) {
      spires(i) = Rational(nums(i), denoms(i))
      aps(i) = new Aprational(new Apint(nums(i)), new Apint(denoms(i)))
      jscis(i) = number.Rational.valueOf(
        number.LargeInteger.valueOf(nums(i)),
        number.LargeInteger.valueOf(denoms(i))
      )
    }

    // set to true to verify that both methods produce the same result
    if (false) assert(verify())
  }

  def spireToAp(r:Rational) = new Aprational(
    new Apint(r.numerator.toBigInt.bigInteger),
    new Apint(r.denominator.toBigInt.bigInteger)
  )

  def spireToJsci(r:Rational) = number.Rational.valueOf(
    number.LargeInteger.valueOf(r.numerator.toBigInt.bigInteger),
    number.LargeInteger.valueOf(r.denominator.toBigInt.bigInteger)
  )

  // this is slow... it probably takes as long as one full run of the benchmark
  def verify() = {
    val len = size
    var i = 0

    var t1 = Rational(0, 1)
    var t2 = new Aprational(new Apint(0), new Apint(1))
    val t3 = number.Rational.valueOf(
      number.LargeInteger.valueOf(0),
      number.LargeInteger.valueOf(1)
    )

    while (i < len) {
      val r = spires(i)
      val a = aps(i)
      val j = jscis(i)

      // compare r and a
      if (spireToAp(r) != a)
        sys.error("items %s and %s differ" format (r, a))
      if (spireToJsci(r) != j)
        sys.error("items %s and %s differ" format (r, j))

      // compare running totals
      val x = t1 + r
      val y = t2.add(a)
      val z = t3.plus(j)
      if (spireToAp(x) != y)
        sys.error("totals %s and %s differ (from %s)" format (x, y, t2))
      if (spireToJsci(x) != z)
        sys.error("totals %s and %s differ (from %s)" format (x, z, t3))

      i += 1
    }
    true
  }

  def timeBuildSpire(reps:Int) = run(reps) {
    val len = size
    val ns = Array.ofDim[Rational](len)
    var i = 0
    while (i < len) {
      ns(i) = Rational(nums(i), denoms(i))
      i += 1
    }
    ns.length
  }

  def timeBuildAp(reps:Int) = run(reps) {
    val len = size
    val ns = Array.ofDim[Aprational](len)
    var i = 0
    while (i < len) {
      ns(i) = new Aprational(new Apint(nums(i)), new Apint(denoms(i)))
      i += 1
    }
    ns.length
  }

  def timeBuildJsci(reps:Int) = run(reps) {
    val len = size
    val ns = Array.ofDim[number.Rational](len)
    var i = 0
    while (i < len) {
      ns(i) = number.Rational.valueOf(
        number.LargeInteger.valueOf(nums(i)),
        number.LargeInteger.valueOf(denoms(i))
      )
      i += 1
    }
    ns.length
  }

  def timeSumSpire(reps:Int) = run(reps) {
    val len = size
    var total = Rational(0, 1)
    var i = 0
    while (i < len) { total += spires(i); i += 1 }
    total
  }

  def timeSumAp(reps:Int) = run(reps) {
    val len = size
    var total = new Aprational(new Apint(0), new Apint(1))
    var i = 0
    while (i < len) { total = total.add(aps(i)); i += 1 }
    total
  }

  def timeSumJsci(reps:Int) = run(reps) {
    val len = size
    var total = number.Rational.ZERO
    var i = 0
    while (i < len) { total = total.plus(jscis(i)); i += 1 }
    total
  }
}
*/