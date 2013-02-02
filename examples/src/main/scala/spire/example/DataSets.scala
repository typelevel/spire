package spire.example

import spire.algebra._
import spire.math.Rational
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ Builder, ListBuffer }

import java.io.{ BufferedReader, InputStreamReader }

import scala.util.Random.shuffle

case class DataSet[V, F, K](name: String, variables: List[Variable[F]], space: CoordinateSpace[V, F], data: List[(V, K)]) {
  def describe: String = {
    import Variable._

    def varType(v: Variable[F]): String = v match {
      case Ignored(_) => "ignored"
      case Continuous(_, _) => "continuous"
      case Categorical(_) => "categorical"
      case Missing(v0, _) => s"${varType(v0)} with missing values"
    }

    val vars = variables.zipWithIndex map { case (v, i) =>
      s"    %2d. ${v.label} (${varType(v)})" format (i + 1)
    } mkString "\n"

    s"""$name - ${data.size} points with ${variables.size} variables (${space.dimensions} effective):
       |$vars""".stripMargin
  }
}

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

  type Output[+K] = (Int, String => K)

  protected def fromLines[CC[_], F, K](lines: List[List[String]],
      variables: List[Variable[F]], out: Output[K])(implicit
      cbf: CanBuildFrom[Nothing, F, CC[F]]): (Int, List[(CC[F], K)]) = {

    // Perform our first pass, building the conversion functions.
    val builders = variables map (_.apply())
    lines foreach { fields =>
      builders zip fields foreach { case (b, s) =>
        b += s
      }
    }

    // Perform our second pass, converting strings to variables.
    val maps = builders map (_.result())
    val (dimensions, datar) = lines.foldLeft((Int.MaxValue, List.empty[(CC[F], K)])) {
      case ((dim, res), fields) =>
        val bldr = cbf()
        val vd = (maps zip fields).foldLeft(0) { case (acc, (f, s)) =>
          val vars = f(s)
          bldr ++= vars
          acc + vars.size
        }
        (math.min(dim, vd), (bldr.result(), out._2(fields(out._1))) :: res)
    }

    (dimensions, datar.reverse)
  }

  def fromResource[CC[_], F, K](name: String, res: String, sep: Char,
      variables: List[Variable[F]], out: Output[K])(
      cs: Int => CoordinateSpace[CC[F], F])(implicit
      cbf: CanBuildFrom[Nothing, F, CC[F]]): DataSet[CC[F], F, K] = {

    val lines = readDataSet(res)
    val (dimensions, data) = fromLines(lines map (_.split(sep).toList), variables, out)(cbf)
    val space = cs(dimensions)

    DataSet[CC[F], F, K](name, variables, space, data)
  }

  import Variable._

  private val IrisVars = List[Variable[Rational]](
    Continuous("Sepal Length", Rational(_)),
    Continuous("Sepal Width", Rational(_)),
    Continuous("Petal Length", Rational(_)),
    Continuous("Petal Width", Rational(_)),
    Ignored("Species"))

  def Iris = fromResource[Vector, Rational, String](
    "Iris", "/datasets/iris.data", ',',
    IrisVars, (4, identity))(CoordinateSpace.seq)

  private val YeastVars = List[Variable[Double]](
    Ignored("Protein"),
    Continuous("mcg", _.toDouble),
    Continuous("gvh", _.toDouble),
    Continuous("alm", _.toDouble),
    Continuous("mit", _.toDouble),
    Continuous("erl", _.toDouble),
    Continuous("pox", _.toDouble),
    Continuous("vac", _.toDouble),
    Continuous("nuc", _.toDouble),
    Ignored("Location"))

  def Yeast = fromResource[Array, Double, String](
    "Yeast", "/datasets/yeast.data", ',',
    YeastVars, (9, identity))(CoordinateSpace.array)

  private val MpgVars = List[Variable[Double]](
    Ignored("MPG"),
    Categorical[Double]("# of Cylinders"),
    Continuous("Displacement", _.toDouble),
    Continuous("Horsepower", _.toDouble).missing("?"),
    Continuous("Weight", _.toDouble),
    Continuous("Acceleration", _.toDouble),
    Continuous("Model Year", _.toDouble),
    Categorical[Double]("Country of Origin"),
    Ignored("Model Name"))

  def MPG = fromResource[Array, Double, Double](
    "MPG", "/datasets/auto-mpg.data", ',',
    MpgVars, (0, _.toDouble))(CoordinateSpace.array)
}

sealed trait Variable[+F] extends CanBuildFrom[Nothing, String, String => List[F]] {
  def label: String

  def apply(n: Nothing): Builder[String, String => List[F]] = apply()

  def missing(sentinel: String): Variable[F] = Variable.Missing(this, sentinel)
}

object Variable {
  protected val Unlabeled = "unnamed variable"

  case class Ignored(label: String = Unlabeled) extends Variable[Nothing] {
    def apply() = new Builder[String, String => List[Nothing]] {
      def += (s: String) = this
      def clear() { }
      def result() = s => Nil
    }
  }

  case class Continuous[+F](label: String = Unlabeled, f: String => F) extends Variable[F] {
    def apply() = new Builder[String, String => List[F]] {
      def += (s: String) = this
      def clear() { }
      def result() = { s => f(s) :: Nil }
    }
  }

  case class Categorical[+F: Ring](label: String = Unlabeled) extends Variable[F] {
    def apply() = new Builder[String, String => List[F]] {
      var categories: Set[String] = Set.empty

      def += (s: String) = {
        categories += s
        this
      }
      def clear() { categories = Set.empty }
      def result() = {
        val orderedCategories = categories.toList

        { s => orderedCategories map (cat => if (cat == s) Ring[F].one else Ring[F].zero) }
      }
    }
  }

  case class Missing[+F](default: Variable[F], sentinel: String) extends Variable[F] {
    def label = default.label

    def apply() = new Builder[String, String => List[F]] {
      val defaultBuilder = default.apply()
      val values: ListBuffer[String] = new ListBuffer[String]

      def += (s: String) = {
        if (s != sentinel) {
          defaultBuilder += s
          values += s
        }
        this
      }
      def clear() { values.clear(); defaultBuilder.clear() }
      def result() = {
        val real = defaultBuilder.result()
        val occurences = values.foldLeft(Map.empty[List[F], Int]) { (acc, v) =>
          val k = real(v)
          acc + (k -> (acc.getOrElse(k, 0) + 1))
        }
        val mostCommon = occurences.maxBy(_._2)._1

        { s => if (s == sentinel) mostCommon else real(s) }
      }
    }
  }
}

object CrossValidation {
  case class Result[V, K](input: V, output: K, predicted: K)

  /**
   * Generic cross-validator that can be provided an arbitrary method to score
   * predictor results.
   */
  def crossValidate[V, F, K](dataset: DataSet[V, F, K], k: Int = 10)(
      train: CoordinateSpace[V, F] => List[(V, K)] => (V => K))(
      score: List[Result[V, K]] => F): F = {
    implicit val field = dataset.space.scalar

    @tailrec
    def loop(left: List[(V, K)], right0: List[(V, K)], n: Int, sum: F): F = {
      if (n <= 0) {
        sum / k
      } else {
        val len = (right0.size + n - 1) / n
        val (removed, right) = right0.splitAt(len)
        val predict = train(dataset.space)(left ++ right)
        val results = removed map { case (in, out) =>
          Result(in, out, predict(in))
        }
        loop(left ++ removed, right, n - 1, sum + score(results))
      }
    }

    loop(Nil, shuffle(dataset.data), k, dataset.space.scalar.zero)
  }

  /**
   * For cross-validating classification, we use the accuracy to score the
   * predictor.
   */
  def crossValidateClassification[V, F, K](dataset: DataSet[V, F, K], k: Int = 10)(
      train: CoordinateSpace[V, F] => List[(V, K)] => (V => K)): F = {
    implicit val field = dataset.space.scalar

    def accuracy(results: List[Result[V, K]]): F = {
      results.foldLeft(field.zero) { case (acc, Result(_, output, predicted)) =>
        acc + (if (predicted == output) field.one else field.zero)
      } / results.size
    }

    crossValidate(dataset, k)(train)(accuracy)
  }

  /**
   * For cross-validating regression, we use the R^2 to score the predictor.
   */
  def crossValidateRegression[V, F](dataset: DataSet[V, F, F], k: Int = 10)(
      train: CoordinateSpace[V, F] => List[(V, F)] => (V => F)): F = {
    implicit val field = dataset.space.scalar

    def rSquared(results: List[Result[V, F]]): F = {
      val mean = results.foldLeft(field.zero)(_ + _.output) / results.size
      val sumSq = results.foldLeft(field.zero) { (acc, result) =>
        acc + (result.output - mean) ** 2
      }
      val error = results.foldLeft(field.zero) { (acc, result) =>
        acc + (result.output - result.predicted) ** 2
      }
      field.one - error / sumSq
    }

    crossValidate(dataset, k)(train)(rSquared)
  }
}
