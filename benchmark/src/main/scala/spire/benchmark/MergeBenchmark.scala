package spire
package benchmark
/*
import spire.implicits._
import spire.math.Rational
import spire.math.BinaryMerge
import spire.math.LinearMerge

import ichi.bench.Thyme

object MergeBenchmark extends App {

  val th = Thyme.warmed(verbose = println)
  // val th = new Thyme()

  val ar = (0 until 1000).map(Rational.apply).toArray
  val br = (1000 until 1001).map(Rational.apply).toArray
  th.pbenchOffWarm("binary merge vs. linear merge (Rational)")(th.Warm(LinearMerge.merge(ar,br)))(th.Warm(BinaryMerge.merge(ar,br)))

  val ai = (0 until 1000).toArray
  val bi = (1000 until 1001).toArray
  th.pbenchOffWarm("binary merge vs. linear merge (Int)")(th.Warm(LinearMerge.merge(ai,bi)))(th.Warm(BinaryMerge.merge(ai,bi)))
}
*/