package spire.example

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

import scala.annotation.tailrec
import scala.util.Random.nextInt

import scala.reflect.ClassTag


/**
 * An example of constructing Random Forests for both regression and
 * classification. This example shows off the utility of vector spaces (in this
 * case `CoordinateSpace`), fields, and orders to create random forests.
 */
object RandomForestExample extends App {

  def test[V, F, K](dataset: DataSet[V, F, K], opts: RandomForest.Options)(implicit
      order: Order[F], classTagV: ClassTag[V], classTagK: ClassTag[K]) {

    println(s"\n${dataset.name} Data Set:")
    println(s"  ${dataset.space.dimensions} variables")
    println(s"  ${dataset.data.size} data points")
    println("  Performing 10-fold cross-validation...")
    val accuracy = DataSets.crossValidate(dataset) { implicit space => data =>
      RandomForest.classification(data, opts)
    }
    println("  Accuracy of %.2f%%" format (accuracy * 100))
  }

  test(DataSets.Iris, RandomForest.Options())
  test(DataSets.Yeast, RandomForest.Options(
    numAxesSample = Some(2), numPointsSample = Some(200),
    numTrees = Some(500), minSplitSize = Some(3)))
}


sealed trait DecisionTree[V, F, K] {
  def apply(v: V)(implicit V: CoordinateSpace[V, F], F: Order[F]): K = {
    @tailrec def loop(tree: DecisionTree[V, F, K]): K = tree match {
      case Split(i, boundary, left, right) =>
        if (v.coord(i) <= boundary) loop(left) else loop(right)
      case Leaf(k) =>
        k
    }

    loop(this)
  }
}

case class Split[V, F, K](variable: Int, boundary: F,
    left: DecisionTree[V, F, K], right: DecisionTree[V, F, K]) extends DecisionTree[V, F, K]

case class Leaf[V, F, K](value: K) extends DecisionTree[V, F, K]


object RandomForest {

  case class Options(
    numAxesSample: Option[Int] = None,
    numPointsSample: Option[Int] = None,
    numTrees: Option[Int] = None,
    minSplitSize: Option[Int] = None)

  object Options {
    def default = Options()
  }

  def regression[V, F](data: Array[V], out: Array[F], options: Options)(implicit
      V: CoordinateSpace[V, F], order: Order[F], ev: ClassTag[V]): V => F = {
    val rfr = new RandomForestRegression[V, F]
    rfr(data, out, options)
  }
      
  def regression[V, F](data: Iterable[V], out: Iterable[F],
      options: Options)(implicit V: CoordinateSpace[V, F], order: Order[F],
      classTagV: ClassTag[V], classTagF: ClassTag[F]): V => F = {
    regression(data.toArray, out.toArray, options)
  }

  def regression[V, F](data: Iterable[(V, F)], options: Options)(implicit
      V: CoordinateSpace[V, F], order: Order[F],
      classTagV: ClassTag[V], classTagF: ClassTag[F]): V => F = {
    val (in, out) = data.unzip
    regression(in.toArray, out.toArray, options)
  }

  def classification[V, F, K](data: Array[V], out: Array[K], options: Options)(implicit
      V: CoordinateSpace[V, F], order: Order[F], ev: ClassTag[V]): V => K = {
    val rfc = new RandomForestClassification[V, F, K]
    rfc(data, out, options)
  }

  def classification[V, F, K](data: Iterable[V], out: Iterable[K],
      options: Options)(implicit V: CoordinateSpace[V, F],
      order: Order[F], classTagV: ClassTag[V], classTagK: ClassTag[K]): V => K = {
    classification(data.toArray, out.toArray, options)
  }

  def classification[V, F, K](data: Iterable[(V, K)], options: Options)(implicit
      V: CoordinateSpace[V, F], order: Order[F],
      classTagV: ClassTag[V], classTagK: ClassTag[K]): V => K = {
    val (in, out) = data.unzip
    classification(in.toArray, out.toArray, options)
  }
}


/**
 * The common bits between regression and classification random forests. The
 * only real difference is how we determine the "disparity" or "error" in a
 * region of the tree. So, our outputs all belong to some type we don't really
 * care about. We then have a way of determining the error of some subset of
 * these outputs using the `Region`.
 */
trait RandomForest[V, F, K] {
  import RandomForest.Options

  implicit def V: CoordinateSpace[V, F]
  implicit def F: Field[F] = V.scalar
  implicit def order: Order[F]
  implicit def vectorClassTag: ClassTag[V]

  // We need to be able to incrementally update the disparity. This is because,
  // for performance reasons, we want to do a linear sweep of some axis in a
  // region, maintaining the disparity of the region before the sweep line and
  // the region after the sweep line. We do this by updating the disparity as
  // the sweep line passes over a point, removing it from one region and adding
  // it to the other.

  protected trait RegionLike {
    def +(k: K): Region
    def -(k: K): Region
    def error: F
    def value: K
  }

  protected trait RegionCompanion {
    def empty: Region
  }

  protected type Region <: RegionLike
  protected def Region: RegionCompanion

  // A forest is just a bunch of trees.

  protected case class Forest(trees: List[DecisionTree[V, F, K]])

  protected case class FixedOptions(
    numAxesSample: Int,
    numPointsSample: Int,
    numTrees: Int,
    minSplitSize: Int)

  /**
   * Construct a random forest.
   */
  protected def randomForest(data: Array[V], outputs: Array[K], opts: FixedOptions): Forest = {
    require(opts.numAxesSample <= V.dimensions, "Cannot sample more dimension than exist in V.")
    require(data.length == outputs.length, "Number of dependent and independent variables must match.")

    // Selects a set of `m` predictors to use as coordiante indices.

    def predictors(): Array[Int] = {
      val indices = new Array[Int](opts.numAxesSample)
      cfor(0)(_ < indices.length, _ + 1) { i => indices(i) = i }
      cfor(V.dimensions - 1)(_ >= indices.length, _ - 1) { i =>
        val j = nextInt(i + 1)
        if (j < indices.length)
          indices(j) = i
      }
      indices
    }

    // Randomly samples `n` points with replacement from `data`. Note that our
    // sample is actually an array of indices.

    def sample(): Array[Int] = {
      val sample = new Array[Int](opts.numPointsSample)
      cfor(0)(_ < sample.length, _ + 1) { i =>
        sample(i) = nextInt(data.length)
      }
      sample
    }

    def region(members: Array[Int]): Region = {
      var d = Region.empty
      cfor(0)(_ < members.length, _ + 1) { i =>
        d += outputs(members(i))
      }
      d
    }

    // Grows a decision tree from a single region. The tree will keep growing
    // until we either hit the minimum region size, or we cannot find a split
    // that will decrease the "disparity" of the region.

    def growTree(members: Array[Int]): DecisionTree[V, F, K] = {
      if (members.length < opts.minSplitSize) {
        Leaf(region(members).value)
      } else {
        val region0 = region(members)
        val vars = predictors()

        var minError = region0.error
        var minVar = -1
        var minIdx = -1

        cfor(0)(_ < vars.length, _ + 1) { i =>
          var axis = vars(i)
          var leftRegion = Region.empty
          var rightRegion = region0

          members.qsortBy(data(_).coord(axis))

          cfor(0)(_ < (members.length - 1), _ + 1) { j =>
            leftRegion += outputs(j)
            rightRegion -= outputs(j)
            val error = (leftRegion.error * (j + 1) + 
                         rightRegion.error * (members.length - j - 1)) / members.length
            if (error < minError) {
              minError = error
              minVar = axis
              minIdx = j
            }
          }
        }

        // If we can never do better than our initial region, we don't bother
        // to split it.

        if (minIdx < 0) {
          minVar = vars(vars.length - 1)
          minIdx = members.length / 2
        }

        members.qsortBy(data(_).coord(minVar))
        val boundary = (data(members(minIdx)).coord(minVar) +
                        data(members(minIdx + 1)).coord(minVar)) / 2
        val left = members take (minIdx + 1)
        val right = members drop (minIdx + 1)
        Split(minVar, boundary, growTree(left), growTree(right))
      }
    }

    Forest((1 to opts.numTrees).map({ _ =>
      growTree(sample())
    }).toList)
  }

  protected def fromForest(forest: Forest): V => K

  protected def defaultOptions(size: Int): FixedOptions

  private def fixOptions(size: Int, options: Options): FixedOptions = {
    val defaults = defaultOptions(size)
    FixedOptions(
      options.numAxesSample getOrElse defaults.numAxesSample,
      options.numPointsSample getOrElse defaults.numPointsSample,
      options.numTrees getOrElse defaults.numTrees,
      options.minSplitSize getOrElse defaults.minSplitSize)
  }

  def apply(data: Array[V], out: Array[K], options: Options) = {
    fromForest(randomForest(data, out, fixOptions(data.length, options)))
  }
}


/**
 * A `RandomForest` implementation for regression. In regression, the output
 * type is assumed to lie in the same field as the input vectors scalars. The
 * final predicted output is the average of the individual tress output (which
 * itself is just the mean of all outputs in the region the point lands in.
 */
class RandomForestRegression[V, F](implicit val V: CoordinateSpace[V, F],
    val order: Order[F], val vectorClassTag: ClassTag[V]) extends RandomForest[V, F, F] {

  // Our "disparity" measure is just the squared error of the region.
  // We could be more careful here and use a "stable" incremental mean and
  // variance, like that described in [1], but this is simpler for now.
  // [1]: http://nfs-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf

  protected final class SquaredError(sum: F, sumSq: F, count: Int) extends RegionLike {
    def +(k: F) = new SquaredError(sum + k, sumSq + (k * k), count + 1)
    def -(k: F) = new SquaredError(sum - k, sumSq - (k * k), count - 1)
    def error: F = {
      val mean = sum / count
      val variance = (sumSq - 2 * mean * sum + mean ** 2) / count
      variance
    }
    def value: F = sum / count
  }

  protected type Region = SquaredError
  object Region extends RegionCompanion {
    def empty = new SquaredError(F.zero, F.zero, 0)
  }

  protected def defaultOptions(size: Int): FixedOptions = {
    val axes = math.max(V.dimensions / 3, math.min(V.dimensions, 2))
    val sampleSize = math.max(size * 2 / 3, 1)
    FixedOptions(axes, sampleSize, size, 5)
  }

  protected def fromForest(forest: Forest): V => F = { v =>
    forest.trees.map(_(v)).qmean
  }
}


/**
 * A `RandomForest` implementation for classification. In this case, the
 * outputs (dependent variable) belongs to some type `K`. This type needs to be
 * a well behaved Java object as its `equals` and `hashCode` will be used to
 * determine equality of classes. This implementation uses a majority vote
 * method to determine classification. Each region in a tree is associated with
 * the most popular class in that region. Ties are broken randomly (not really).
 * Within a forest, each tree casts its vote for classification of a point and
 * the majority wins. Again, ties are broken randomly (again, not really).
 */
class RandomForestClassification[V, F, K](implicit val V: CoordinateSpace[V, F],
    val order: Order[F], val vectorClassTag: ClassTag[V]) extends RandomForest[V, F, K] {

  // Our "disparity" measure here is the Gini index. It basically measures how
  // homogeneous our region is, giving regions of high variability higher
  // scores.

  protected final class GiniIndex(m: Map[K, Int]) extends RegionLike {
    def +(k: K) = new GiniIndex(m + (k -> (m.getOrElse(k, 0) + 1)))
    def -(k: K) = new GiniIndex(m + (k -> (m.getOrElse(k, 0) - 1)))
    def error: F = {
      val n = F.fromInt(m.foldLeft(0)(_ + _._2))
      m.foldLeft(F.zero) { case (idx, (k, cnt)) =>
        idx + (F.fromInt(cnt) / n)
      }
    }
    def value: K = m.maxBy(_._2)._1
  }

  protected type Region = GiniIndex
  object Region extends RegionCompanion {
    def empty = new GiniIndex(Map.empty)
  }

  protected def defaultOptions(size: Int): FixedOptions = {
    val axes = math.max(math.sqrt(V.dimensions.toDouble).toInt, math.min(V.dimensions, 2))
    val sampleSize = math.max(size * 2 / 3, 1)
    FixedOptions(axes, sampleSize, size, 5)
  }

  protected def fromForest(forest: Forest): V => K = { v =>
    forest.trees.foldLeft(Map.empty[K, Int]) { (acc, classify) =>
      val k = classify(v)
      acc + (k -> (acc.getOrElse(k, 0) + 1))
    }.maxBy(_._2)._1
  }
}
