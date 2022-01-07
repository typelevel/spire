/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package example

import spire.algebra._
import spire.implicits._
import scala.collection.Factory

import scala.util.Random.{nextDouble, nextGaussian, nextInt}

/**
 * An example using `NormedVectorSpace`s to create a generic k-Means implementation. We also abstract over the
 * collection type for the fun of it. We implement Lloyd's algorithm, which has problems of its own, but performs well
 * enough.
 */
object KMeansExample extends App {

  /**
   * Returns a collection of k points which are the centers of k clusters of `points0`.
   */
  def kMeans[V, @sp(Double) A, CC[V] <: Iterable[V]](points0: CC[V],
                                                     k: Int
  )(implicit vs: NormedVectorSpace[V, A], order: Order[A], cbf: Factory[V, CC[V]], ct: ClassTag[V]): CC[V] = {

    val points = points0.toArray

    // We want to create an array that maps the index of each point to the
    // index of the cluster it is closest to (according to the norm).

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

    // This is the main loop of the k-means algorithm. Given a new clustering
    // and some previous assignments mapping each point to a cluster, we
    // determine if the new clustering will cause any points to switch
    // clusters. If so, we re-assign all points to their closest center in the
    // clustering, then find new centers using the centroids of the points
    // assigned to each cluster, rinse, repeat.

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
          clusters(idx) = clusters(idx) + points(i)
          counts(idx) += 1
        }
        cfor(0)(_ < clusters.length, _ + 1) { j =>
          clusters(j) = clusters(j) :/ vs.scalar.fromInt(counts(j))
        }
        loop(assignments, clusters)
      }
    }

    // Our seed points are chosen rather naively. However, the points below are
    // generated randomly, so we don't need to worry about being too smart here.

    val init: Array[V] = points.take(k)
    val clusters = loop(assign(init), init)

    // We work with arrays above, but turn it into the collection type the user
    // wants before we return the clusters.

    val bldr = cbf.newBuilder
    cfor(0)(_ < clusters.length, _ + 1) { i =>
      bldr += clusters(i)
    }
    bldr.result()
  }

  // This method let's us generate a set of n points which are clustered around
  // k centers in d-dimensions.

  def genPoints[CC[_], V, @sp(Double) A](d: Int, k: Int, n: Int)(
    f: Array[Double] => V
  )(implicit vs: VectorSpace[V, A], cbf: Factory[V, CC[V]]): CC[V] = {

    def randPoint(gen: => Double): V = f((1 to d).map(_ => gen).toArray)

    val centers: Vector[V] = (1 to k).map { _ =>
      randPoint(nextDouble() * 10)
    }.toVector

    val bldr = cbf.newBuilder
    cfor(0)(_ < n, _ + 1) { _ =>
      bldr += centers(nextInt(k)) + randPoint(nextGaussian())
    }
    bldr.result()
  }

  implicit val mc: java.math.MathContext = java.math.MathContext.DECIMAL128

  // We construct 3 sets of points, each using different VectorSpaces. We'll
  // cluster each one, using the same k-means algorithm.

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
