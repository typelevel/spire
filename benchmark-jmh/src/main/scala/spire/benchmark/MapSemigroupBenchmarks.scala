package spire
package benchmark
/*
import spire.algebra._
import spire.implicits._

import com.google.caliper.Param

import scala.util.Random._

object MapSemigroupBenchmarks extends MyRunner(classOf[MapSemigroupBenchmarks])

class MapSemigroupBenchmarks extends MyBenchmark with BenchmarkData {

  // From Algebird -- note that deciding equality is not, in general,
  // decidable. In Spire, we choose to separate equality out.
  //final def isNonZero[K, V](x: Map[K,V])(implicit semigroup: Semigroup[V], eq: Eq[V]) = {
  //  !x.isEmpty && x.valuesIterator.exists { v =>
  //    eq.eqv(v, semigroup.id)
  //  }
  //}

  /**
   * Original code from Algebird, though without the isNonZero stuff. That is
   * not, in general, deciable.
   */
  def algebirdAdd[K, V](x: Map[K, V], y: Map[K, V])(implicit
      semigroup: Semigroup[V], eq: Eq[V]): Map[K, V] = {
    val (big, small, bigOnLeft) = if(x.size > y.size) { (x,y,true) } else { (y,x,false) }
    small.foldLeft(big) { (oldMap, kv) =>
      val newV = big
        .get(kv._1)
        .map { bigV =>
          if(bigOnLeft)
            semigroup.combine(bigV, kv._2)
          else
            semigroup.combine(kv._2, bigV)
        }
        .getOrElse(kv._2)
      oldMap + (kv._1 -> newV)
    }
  }



  @inline private final def add[K, V](x: Map[K, V], y: Map[K, V], flip: Boolean)(implicit
      semigroup: Semigroup[V]): Map[K, V] = {
    y.foldLeft(x) { case (z, kv) =>
      z + ((kv._1, (x get kv._1) match {
        case Some(u) => if (flip) semigroup.combine(kv._2, u) else semigroup.combine(u, kv._2)
        case None => kv._2
      }))
    }
  }

  def bulkAdd[K, V](x: Map[K, V], y: Map[K, V])(implicit
      semigroup: Semigroup[V]): Map[K, V] = {
    if (x.size < y.size) add(y, x, true) else add(x, y, false)
  }

  def spireAdd[K, V](x: Map[K, V], y: Map[K, V])(implicit
      rng: Rng[Map[K, V]]): Map[K, V] = rng.plus(x, y)

  val numMaps = 1000

  @Param(Array("2", "4", "8", "16", "32", "64"))
  var mapSize: Int = 0

  @Param(Array("random", "sparse", "dense"))
  var mapType: String = null

  var maps: Array[Map[Int, Int]] = null

  def genMaps(gen: Int => (Int, Int)): Array[Map[Int, Int]] = {
    val arr = new Array[Map[Int, Int]](numMaps)
    var i = 0
    (0 until numMaps) foreach { i =>
      arr(i) = Map((1 to mapSize) map gen: _*)
    }
    arr
  }

  override protected def setUp(): Unit = {
    if (mapType == "random") {
      maps = genMaps { i => (nextInt, nextInt) }
    } else if (mapType == "sparse") {
      maps = genMaps { i => (nextInt(mapSize), nextInt) }
    } else if (mapType == "dense") {
      maps = genMaps { i => (i, nextInt) }
    } else {
      sys.error("What are you doing to me!")
    }
  }

  implicit val semigroup = new Semigroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
  }

  def timeAlgebirdMapAdd(reps: Int) = run(reps) {
    var i = 1
    var total = 0
    while (i < numMaps) {
      total += algebirdAdd(maps(i - 1), maps(i)).size
      i += 1
    }
  }

  def timeBulkMapAdd(reps: Int) = run(reps) {
    var i = 1
    var total = 0
    while (i < numMaps) {
      total += bulkAdd(maps(i - 1), maps(i)).size
      i += 1
    }
  }

  def timeSpireAdd(reps: Int) = run(reps) {
    var i = 1
    var total = 0
    while (i < numMaps) {
      total += spireAdd(maps(i - 1), maps(i)).size
      i += 1
    }
  }
}
*/