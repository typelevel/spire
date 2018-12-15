package spire
package benchmark
/*
import scala.util.Random
import Random._

import spire.math._
import spire.implicits._

import com.google.caliper.Param
import org.apache.commons.math3.analysis.polynomials._

object PolynomialBenchmarks extends MyRunner(classOf[PolynomialBenchmarks])

class PolynomialBenchmarks extends MyBenchmark {

  @Param(Array("1", "2", "4", "8", "16"))
  var size: Int = 0

  def arbitraryRational = {
    val d = nextLong() % 100
    Rational(nextLong(), if (d == 0L) 1L else d)
  }

  var spireDenseRationalPolys: Array[Polynomial[Rational]] = null
  var spireSparseRationalPolys: Array[Polynomial[Rational]] = null
  var spireDenseDoublePolys: Array[Polynomial[Double]] = null
  var spireSparseDoublePolys: Array[Polynomial[Double]] = null
  var commonsDoublePolys: Array[PolynomialFunction] = null

  override protected def setUp(): Unit = {

    val coeffs: Array[Array[Rational]] =
      init(100)(init(size)(arbitraryRational))

    spireDenseRationalPolys = coeffs.map(cs => Polynomial.dense(cs))
    spireSparseRationalPolys = spireDenseRationalPolys.map(_.toSparse)
    spireDenseDoublePolys = coeffs.map(cs => Polynomial.dense(cs.map(_.toDouble)))
    spireSparseDoublePolys = spireDenseDoublePolys.map(_.toSparse)
    commonsDoublePolys = coeffs.map(cs => new PolynomialFunction(cs.map(_.toDouble)))
  }

  def addSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total: Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) + data(i); i += 1 }
    total
  }

  def addSpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total: Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) + data(i); i += 1 }
    total
  }

  def addCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total: PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0).add(data(i)); i += 1 }
    total
  }

  def multiplySpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total: Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) * data(i); i += 1 }
    total
  }

  def multiplySpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total: Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) * data(i); i += 1 }
    total
  }

  def multiplyCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total: PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0).multiply(data(i)); i += 1 }
    total
  }

  def derivativeSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Polynomial[Rational] = {
    var total : Polynomial[Rational] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).derivative; i += 1 }
    total
  }

  def derivativeSpireDoublePolynomials(data: Array[Polynomial[Double]]): Polynomial[Double] = {
    var total : Polynomial[Double] = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).derivative; i += 1 }
    total
  }

  def derivativeCommonsDoublePolynomials(data: Array[PolynomialFunction]): PolynomialFunction = {
    var total : PolynomialFunction = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).polynomialDerivative; i += 1 }
    total
  }

  def evaluateSpireRationalPolynomials(data: Array[Polynomial[Rational]]): Rational = {
    val testVariable = Rational(2, 1)
    var total : Rational = Rational(1,1)
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).apply(testVariable); i += 1 }
    total
  }

  def evaluateSpireDoublePolynomials(data: Array[Polynomial[Double]]): Double = {
    val testVariable = 2.0
    var total : Double = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).apply(testVariable); i += 1 }
    total
  }

  def evaluateCommonsDoublePolynomials(data: Array[PolynomialFunction]): Double = {
    val testVariable = 2.0
    var total : Double = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total = data(i).value(testVariable); i += 1 }
    total
  }

  def quotModSpireRationalPolynomials(data: Array[Polynomial[Rational]]): (Polynomial[Rational], Polynomial[Rational]) = {
    var total: (Polynomial[Rational], Polynomial[Rational]) = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) equotmod data(i); i += 1 }
    total
  }

  def quotModSpireDoublePolynomials(data: Array[Polynomial[Double]]): (Polynomial[Double], Polynomial[Double]) = {
    var total: (Polynomial[Double], Polynomial[Double]) = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) equotmod data(i); i += 1 }
    total
  }

  def timeAddSpireRationalPolysSparse(reps: Int) = run(reps)(addSpireRationalPolynomials(spireSparseRationalPolys))
  def timeAddSpireRationalPolysDense(reps: Int) = run(reps)(addSpireRationalPolynomials(spireDenseRationalPolys))
  def timeAddSpireDoublePolysSparse(reps: Int) = run(reps)(addSpireDoublePolynomials(spireSparseDoublePolys))
  def timeAddSpireDoublePolysDense(reps: Int) = run(reps)(addSpireDoublePolynomials(spireDenseDoublePolys))
  def timeAddCommonsDoublePolynomials(reps: Int) = run(reps)(addCommonsDoublePolynomials(commonsDoublePolys))

  def timeMultiplySpireRationalPolysSparse(reps: Int) = run(reps)(multiplySpireRationalPolynomials(spireSparseRationalPolys))
  def timeMultiplySpireRationalPolysDense(reps: Int) = run(reps)(multiplySpireRationalPolynomials(spireDenseRationalPolys))
  def timeMultiplySpireDoublePolysSparse(reps: Int) = run(reps)(multiplySpireDoublePolynomials(spireSparseDoublePolys))
  def timeMultiplySpireDoublePolysDense(reps: Int) = run(reps)(multiplySpireDoublePolynomials(spireDenseDoublePolys))
  def timeMultiplyCommonsDoublePolynomials(reps: Int) = run(reps)(multiplyCommonsDoublePolynomials(commonsDoublePolys))

  def timeDerivativeSpireRationalPolysSparse(reps: Int) = run(reps)(derivativeSpireRationalPolynomials(spireSparseRationalPolys))
  def timeDerivativeSpireRationalPolysDense(reps: Int) = run(reps)(derivativeSpireRationalPolynomials(spireDenseRationalPolys))
  def timeDerivativeSpireDoublePolysSparse(reps: Int) = run(reps)(derivativeSpireDoublePolynomials(spireSparseDoublePolys))
  def timeDerivativeSpireDoublePolysDense(reps: Int) = run(reps)(derivativeSpireDoublePolynomials(spireDenseDoublePolys))
  def timeDerivativeCommonsDoublePolynomials(reps: Int) = run(reps)(derivativeCommonsDoublePolynomials(commonsDoublePolys))

  def timeEvaluateSpireRationalPolysSparse(reps: Int) = run(reps)(evaluateSpireRationalPolynomials(spireSparseRationalPolys))
  def timeEvaluateSpireRationalPolysDense(reps: Int) = run(reps)(evaluateSpireRationalPolynomials(spireDenseRationalPolys))
  def timeEvaluateSpireDoublePolysSparse(reps: Int) = run(reps)(evaluateSpireDoublePolynomials(spireSparseDoublePolys))
  def timeEvaluateSpireDoublePolysDense(reps: Int) = run(reps)(evaluateSpireDoublePolynomials(spireDenseDoublePolys))
  def timeEvaluateCommonsDoublePolynomials(reps: Int) = run(reps)(evaluateCommonsDoublePolynomials(commonsDoublePolys))

  def timeQuotModSpireRationalPolysSparse(reps: Int) = run(reps)(quotModSpireRationalPolynomials(spireSparseRationalPolys))
  def timeQuotModSpireRationalPolysDense(reps: Int) = run(reps)(quotModSpireRationalPolynomials(spireDenseRationalPolys))
  def timeQuotModSpireDoublePolysSparse(reps: Int) = run(reps)(quotModSpireDoublePolynomials(spireSparseDoublePolys))
  def timeQuotModSpireDoublePolysDense(reps: Int) = run(reps)(quotModSpireDoublePolynomials(spireDenseDoublePolys))

}
*/