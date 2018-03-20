package spire.stats.test

import scala.annotation.tailrec
import scala.reflect.ClassTag
import spire.algebra._
import spire.stats.distribution._

object AndersonDarling {
  import spire.syntax.field._
  import spire.syntax.trig._
  import spire.syntax.isReal._
  import spire.syntax.std.array._

  def apply[A: Field: Trig: IsReal: ClassTag, B <: ContinuousDistribution[A]](xs: Array[A], dist: B): (A, Seq[A], Seq[A]) = {
    val cxs = xs.clone

    @tailrec def loop(sum: A, i: Int, a: A, b: A): A = if (i < cxs.length) {
      val y = dist.cdf(cxs(i))
      val k = a * y.log + b * (1 - y).log
      loop(sum + k, i + 1, a + 2, b - 2)
    } else sum

    cxs.qsort
    val n = Field[A].fromInt(cxs.length)
    val sum = loop(Field[A].zero, 0, Field[A].one, 2 * n - 1)
    val statistic = -n - sum / n
    val significanceLevels = Seq(0.15, 0.10, 0.05, 0.025, 0.01).map(Field[A].fromDouble)
    val criticalValues = Seq(1.610, 1.933, 2.492, 3.070, 3.857).map(Field[A].fromDouble)
    
    (statistic, significanceLevels, criticalValues)
  }

  def rejectedAt[A: Field: Trig: IsReal: ClassTag, B <: ContinuousDistribution[A]](xs: Array[A], dist: B, significanceLevel: A): Boolean = {
    val (statistic, significanceLevels, criticalValues) = apply(xs, dist)
    
    val res = (significanceLevels zip criticalValues).find { case (sl, _) => sl == significanceLevel }

    res match {
      case Some((_, cv)) => statistic > cv
      case _ => ???
    }
  }
}

