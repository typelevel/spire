package spire.benchmark

import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._

import com.google.caliper.{ Runner, SimpleBenchmark, Param }
import org.apache.commons.math3.analysis.polynomials._
import org.apache.commons.math3.analysis.UnivariateFunction

object PolynomialBenchmarks extends MyRunner(classOf[PolynomialBenchmarks])

class PolynomialBenchmarks extends MyBenchmark {

  // @Param(Array("2", "10", "100", "1000", "10000"))
  @Param(Array("100"))
  var size: Int = 0

  def arbitraryRational = {
    val (n, d) = (nextLong() % 100, nextLong() % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  }

  var spireTermArraysRational: Array[Array[Term[Rational]]] = null
  var spireTermArraysDouble: Array[Array[Term[Double]]] = null
  var commonsArraysDoubleArrays: Array[Array[Double]] = null
  var spireRationalPolys: Array[Polynomial[Rational]] = null
  var spireDoublePolys: Array[Polynomial[Double]] = null
  var commonsDoublePolys: Array[PolynomialFunction] = null

  override protected def setUp() {

    spireTermArraysRational = init(size)((
      for(i <- 0 until 10) yield Term(arbitraryRational, i)
      ).toArray)
    spireTermArraysDouble = spireTermArraysRational.map(_.map(t => Term(t.coeff.toDouble, t.exp)))
    commonsArraysDoubleArrays = spireTermArraysRational.map(_.map(t => t.coeff.toDouble))
    spireRationalPolys = spireTermArraysRational.map(Polynomial(_))
    spireDoublePolys = spireTermArraysDouble.map(Polynomial(_))
    commonsDoublePolys = commonsArraysDoubleArrays.map(darr => new PolynomialFunction(darr))

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
    while (i < len) { total = data(0) /% data(i); i += 1 }
    total
  }

  def quotModSpireDoublePolynomials(data: Array[Polynomial[Double]]): (Polynomial[Double], Polynomial[Double]) = {
    var total: (Polynomial[Double], Polynomial[Double]) = null
    var i = 0
    val len = data.length
    while (i < len) { total = data(0) /% data(i); i += 1 }
    total
  }


  def timeAddSpireRationalPolys(reps: Int) = run(reps)(addSpireRationalPolynomials(spireRationalPolys))
  def timeAddSpireDoublePolys(reps: Int) = run(reps)(addSpireDoublePolynomials(spireDoublePolys))
  def timeAddCommonsDoublePolynomials(reps: Int) = run(reps)(addCommonsDoublePolynomials(commonsDoublePolys))

  def timeMultiplySpireRationalPolys(reps: Int) = run(reps)(multiplySpireRationalPolynomials(spireRationalPolys))
  def timeMultiplySpireDoublePolys(reps: Int) = run(reps)(multiplySpireDoublePolynomials(spireDoublePolys))
  def timeMultiplyCommonsDoublePolynomials(reps: Int) = run(reps)(multiplyCommonsDoublePolynomials(commonsDoublePolys))
  
  // def timeDerivativeSpireRationalPolys(reps: Int) = run(reps)(derivativeSpireRationalPolynomials(spireRationalPolys))
  // def timeDerivativeSpireDoublePolys(reps: Int) = run(reps)(derivativeSpireDoublePolynomials(spireDoublePolys))
  // def timeDerivativeCommonsDoublePolynomials(reps: Int) = run(reps)(derivativeCommonsDoublePolynomials(commonsDoublePolys))
  
  // def timeEvaluateSpireRationalPolys(reps: Int) = run(reps)(evaluateSpireRationalPolynomials(spireRationalPolys))
  // def timeEvaluateSpireDoublePolys(reps: Int) = run(reps)(evaluateSpireDoublePolynomials(spireDoublePolys))
  // def timeEvaluateCommonsDoublePolynomials(reps: Int) = run(reps)(evaluateCommonsDoublePolynomials(commonsDoublePolys))

  // def timeQuotModSpireRationalPolys(reps: Int) = run(reps)(quotModSpireRationalPolynomials(spireRationalPolys))
  // def timeQuotModSpireDoublePolys(reps: Int) = run(reps)(quotModSpireDoublePolynomials(spireDoublePolys))

}
