package spire.example

import spire.algebra._
import spire.math.Rational

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

import java.io.{ BufferedReader, InputStreamReader }

import scala.util.Random.shuffle

case class DataSet[V, F, K](name: String, space: CoordinateSpace[V, F], data: List[(V, K)])

object DataSet {
  private def withResource[A](path: String)(f: BufferedReader => A): A = {
    val in = getClass.getResourceAsStream(path)
    val reader = new BufferedReader(new InputStreamReader(in))
    val result = f(reader)
    reader.close()
    result
  }

  private def readDataSet(path: String): List[String] = withResource(path) { reader =>
    Stream.continually(reader.readLine()).takeWhile(_ != null).toList
  }

  /**
   * The Iris data set. It has 4 variables and 3 classes (represented as
   * strings). We use `Rational` for the underlying field, wrapped in a
   * `Vector`.
   */
  lazy val Iris: DataSet[Vector[Rational], Rational, String] = {
    val lines = readDataSet("/datasets/iris.data")
    val data = lines map { line =>
      val fields = line.split(',').toVector
      (fields take 4 map (Rational(_)), fields.last)
    }

    DataSet("Iris", CoordinateSpace.seq[Rational, Vector](4), data)
  }

  /**
   * The Yeast data set. It has 8 variables and several `String` classes. We
   * use `Array`s of `Double`s to store the points in.
   */
  lazy val Yeast: DataSet[Array[Double], Double, String] = {
    val lines = readDataSet("/datasets/yeast.data")
    val data = lines map { line =>
      val fields = line.split(',').toArray
      (fields drop 1 take 8 map (_.toDouble), fields.last)
    }

    DataSet("Yeast", CoordinateSpace.array[Double](8), data)
  }

  def crossValidate[V, F, K](dataset: DataSet[V, F, K], k: Int = 10)(
      train: CoordinateSpace[V, F] => List[(V, K)] => (V => K)): Double = {

    @tailrec
    def loop(left: List[(V, K)], right0: List[(V, K)], n: Int, sum: Double = 0.0): Double = {
      if (n <= 0) {
        sum / k
      } else {
        val len = (right0.size + n - 1) / n
        val (removed, right) = right0.splitAt(len)
        val classify = train(dataset.space)(left ++ right)
        val accuracy = removed.foldLeft(0.0) { case (acc, (v, klass)) =>
          acc + (if (classify(v) == klass) 1.0 else 0.0)
        } / removed.size
        loop(left ++ removed, right, n - 1, sum + accuracy)
      }
    }

    loop(Nil, shuffle(dataset.data), k)
  }
}
