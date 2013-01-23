package spire.example

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.{ specialized => spec }
import scala.util.Random.{ nextInt, nextDouble, nextGaussian }
import scala.annotation.tailrec

/**
 * An example using `NormedVectorSpace`s to create a generic k-Means
 * implementation. We also abstract over the collection type for the fun of it.
 * We implement Lloyd's algorithm, which has problems of its own, but performs
 * well enough.
 */
object lloyd extends App {
  def kMeans[V, @spec(Double) A, CC[V] <: Iterable[V]](points0: CC[V], k: Int)(implicit
      vs: NormedVectorSpace[V, A], order: Order[A],
      cbf: CanBuildFrom[Nothing, V, CC[V]], ct: ClassTag[V]): CC[V] = {

    val points = points0.toArray

    def assign(clusters: Array[V]): Array[Int] = {
      val assignments = new Array[Int](points.length)
      cfor(0)(_ < points.length, _ + 1) { i =>
        var min = (points(i) - clusters(0)).norm
        var idx = 0
        cfor(1)(_ < clusters.length, _ + 1) { j =>
          val dist = (points(i) - clusters(j)).norm
          if (dist < min) {
            min = dist
            idx = j
          }
        }
        assignments(i) = idx
      }
      assignments
    }

    @tailrec
    def loop(assignments0: Array[Int], clusters0: Array[V]): Array[V] = {
      val assignments = assign(clusters0)
      if (assignments === assignments0) {
        clusters0
      } else {
        val clusters = Array.fill[V](clusters0.length)(vs.zero)
        val counts = new Array[Int](clusters0.length)
        cfor(0)(_ < points.length, _ + 1) { i =>
          val idx = assignments(i)
          clusters(idx) = clusters(i) + points(i)
          counts(idx) += 1
        }
        cfor(0)(_ < clusters.length, _ + 1) { j =>
          clusters(j) = clusters(j) :/ vs.scalar.fromInt(counts(j))
        }
        loop(assignments, clusters)
      }
    }

    val init: Array[V] = points take k
    val clusters = loop(assign(init), init)

    val bldr = cbf()
    cfor(0)(_ < clusters.length, _ + 1) { i =>
      bldr += clusters(i)
    }
    bldr.result()
  }

  def genPoints[CC[_], V, @spec(Double) A](d: Int, k: Int, n: Int)(f: Array[Double] => V)(implicit
      vs: VectorSpace[V, A], cbf: CanBuildFrom[Nothing, V, CC[V]]): CC[V] = {

    def randPoint(gen: => Double): V = f((1 to d).map(_ => gen)(collection.breakOut))

    val centers: Vector[V] = (1 to k).map({ _ =>
      randPoint(nextDouble() * 10)
    })(collection.breakOut)

    val bldr = cbf()
    cfor(0)(_ < n, _ + 1) { _ =>
      bldr += centers(nextInt(k)) + randPoint(nextGaussian)
    }
    bldr.result()
  }

  implicit val mc = java.math.MathContext.DECIMAL128

  val points0 = genPoints[List, Array[Double], Double](15, 5, 10000)(identity)
  val points1 = genPoints[List, Vector[Double], Double](5, 10, 10000)(_.toVector)
  val points2 = genPoints[List, Vector[BigDecimal], BigDecimal](7, 8, 2000)(_.map(BigDecimal(_)).toVector)

  println("Finding clusters of Array[Double] points.")
  val cluster0 = kMeans(points0, 5)

  println("Finding clusters of Vector[Double] points.")
  val cluster1 = kMeans(points1, 10)

  println("Finding clusters of Vector[BigDecimal] points.")
  val cluster2 = kMeans(points2, 8)

  println("Finished finding our clusters! Yay!")
}
