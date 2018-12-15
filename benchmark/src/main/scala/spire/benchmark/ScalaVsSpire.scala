package spire
package benchmark

import scala.util.Random
import Random._

import spire.algebra._
import spire.std.any._

import scala.math.{Numeric => ScalaN}
/*
import com.google.caliper.Param

object ScalaVsSpireBenchmarks extends MyRunner(classOf[ScalaVsSpireBenchmarks])

class ScalaVsSpireBenchmarks extends MyBenchmark {
  //@Param(Array("10", "15", "20", "25"))
  @Param(Array("20"))
  var pow: Int = 0

  var size: Int = 0

  var as: Array[Int] = null
  var bs: Array[Int] = null
  var cs: Array[Int] = null

  override protected def setUp(): Unit = {
    size = spire.math.pow(2, pow).toInt
    //as = init(size)(scala.math.abs(nextInt).toInt % 1000 + 1)
    //bs = init(size)(scala.math.abs(nextInt).toInt % 1000 + 1)
    as = init(size)(scala.math.abs(nextInt).toInt % 100000 + 1)
    bs = init(size)(scala.math.abs(nextInt).toInt % 100000 + 1)
    cs = new Array(size)
  }

  def timePairwiseDirect(reps:Int) = run(reps)(doPairwiseDirect(as, bs, cs))
  def timePairwiseGeneric(reps:Int) = run(reps)(doPairwiseGeneric(as, bs, cs))
  def timePairwiseSpire(reps:Int) = run(reps)(doPairwiseSpire(as, bs, cs))

  def timeIncrementDirect(reps:Int) = run(reps)(doIncrementDirect(0, size))
  def timeIncrementGeneric(reps:Int) = run(reps)(doIncrementGeneric(0, size))
  def timeIncrementSpire(reps:Int) = run(reps)(doIncrementSpire(0, size))

  def timeMinMaxDirect(reps:Int) = run(reps)(doMinMaxDirect(as))
  def timeMinMaxGeneric(reps:Int) = run(reps)(doMinMaxGeneric(as))
  def timeMinMaxSpire(reps:Int) = run(reps)(doMinMaxSpire(as))

  def timeGcdDirect(reps:Int) = run(reps)(doGcdDirect(as, bs, cs))
  def timeGcdGeneric(reps:Int) = run(reps)(doGcdGeneric(as, bs, cs))
  def timeGcdSpire(reps:Int) = run(reps)(doGcdSpire(as, bs, cs))

  def timeScaleDirect(reps:Int) = run(reps)(doScaleDirect(as, 9, 4, cs))
  def timeScaleGeneric(reps:Int) = run(reps)(doScaleGeneric(as, 9, 4, cs))
  def timeScaleSpire(reps:Int) = run(reps)(doScaleSpire(as, 9, 4, cs))

  /**
   * Pairwise addition between arrays
   */
  def doPairwiseDirect(as: Array[Int], bs: Array[Int], cs: Array[Int]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  def doPairwiseGeneric[A:ScalaN](as:Array[A], bs: Array[A], cs: Array[A]): Unit = {
    import ScalaN.Implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  def doPairwiseSpire[@sp(Int) A:Ring](as:Array[A], bs: Array[A], cs: Array[A]): Unit = {
    import spire.implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  /**
   * Simple incrementing counter
   */
  def doIncrementDirect(start: Int, n: Int) = {
    var t = start
    var i = 0
    while (i < n) { t += 1; i += 1 }
    t
  }

  def doIncrementGeneric[A:ScalaN](start: A, n: A) = {
    import ScalaN.Implicits._
    val ev = implicitly[ScalaN[A]]
    import ev.mkOrderingOps
    var t = start
    var i = ev.zero
    while (i < n) { t += ev.one; i += ev.one }
    t
  }

  def doIncrementSpire[@sp(Int) A:Ring:Order](start: A, n: A) = {
    import spire.implicits._
    val ev = Ring[A]
    var t = start
    var i = ev.zero
    while (i < n) { t += ev.one; i += ev.one }
    t
  }

  /**
   * Find min/max values.
   */
  def doMinMaxDirect(ns: Array[Int]) = {
    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  def doMinMaxGeneric[A:ScalaN](ns: Array[A]) = {
    val ev = implicitly[ScalaN[A]]
    import ev.mkOrderingOps

    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  def doMinMaxSpire[@sp(Int) A:Ring:Order](ns: Array[A]) = {
    import spire.implicits._

    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  /**
   * Find GCD.
   */
  @tailrec final def gcdDirect(a: Int, b: Int): Int =
    if (a % b == 0) b else gcdDirect(b, a % b)

  def doGcdDirect(as: Array[Int], bs: Array[Int], cs: Array[Int]) = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdDirect(as(i), bs(i)); i += 1 }
  }

  import scala.math.{Integral => ScalaI}
  @tailrec final def gcdGeneric[A](a: A, b: A)(implicit ev:ScalaI[A]): A = {
    import ScalaI.Implicits._
    if (a % b == ev.zero) b else gcdGeneric(b, a % b)
  }

  def doGcdGeneric[A:ScalaI](as: Array[A], bs: Array[A], cs: Array[A]) = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdGeneric(as(i), bs(i)); i += 1 }
  }

  @tailrec final def gcdSpire[@sp(Int) A](a: A, b: A)(implicit ev1:EuclideanRing[A], ev2:Eq[A]): A = {
    import spire.implicits._
    if ((a emod b) === ev1.zero) b else gcdSpire(b, a emod b)
  }

  def doGcdSpire[@sp(Int) A:EuclideanRing:Eq](as: Array[A], bs: Array[A], cs: Array[A]) = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdSpire(as(i), bs(i)); i += 1 }
  }

  /**
   * Scale array.
   */
  def doScaleDirect(as: Array[Int], n: Int, d: Int, cs: Array[Int]) = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) * n / d; i += 1}
  }

  def doScaleGeneric[A:ScalaI](as: Array[A], n: A, d: A, cs: Array[A]) = {
    import ScalaI.Implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) * n / d; i += 1}
  }

  def doScaleSpire[@sp(Int) A:EuclideanRing](as: Array[A], n: A, d: A, cs: Array[A]) = {
    import spire.implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) * n equot d; i += 1}
  }
}

//
//object Direct {
//  @tailrec final def gcd(a: Int, b: Int): Int =
//    if (a % b == 0) b else gcd(b, a % b)
//}
//
//object Spire {
//  @tailrec final def gcd[@sp(Int) A: Integral](a: A, b: A): A =
//    if (a % b === Integral[A].zero) b else gcd(b, a % b)
//}
//
//object Scala {
//  @tailrec final def gcd[A: Integral](a: A, b: A): A =
//    if (a % b == implicitly[Integra[A]].zero) b else gcd(b, a % b)
//}
*/