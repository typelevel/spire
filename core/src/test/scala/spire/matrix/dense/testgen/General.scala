package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Transposition._
import UpperOrLower._
import Sides._
import DiagonalProperty._
import spire.matrix.NumericPropertiesOfDouble
import spire.matrix.dense.BLAS
import spire.matrix.dense.random._
import spire.implicits._
import scala.math.{sqrt, min, max}


/**
 * Dimension sampling
 */
class TestDimensions(nonSpecialDimensions:Int=0)
                    (implicit gen:Defaults.IntegerGenerator) {

  def oneDimensionSample =
    Iterator(1, 2, 3, 5, 10, 16) ++
    Iterator.fill(nonSpecialDimensions)(gen.nextInt(32, 64))

  def twoDimensionSample =
    for(m <- oneDimensionSample; n <- oneDimensionSample) yield (m,n)
}

/**
 * Uncorrelated elements drawn from the same distribution to build scalar,
 * vector, and matrix samples.
 */
class RandomUncorrelatedElements(
  nonSpecialDimensions:Int=0,
  nonSpecialScalars:Int=1,
  matricesPerDimensions:Int=1,
  elements:ScalarDistribution,
  scalars:ScalarDistribution=null)
  (implicit gen:Defaults.IntegerGenerator)
extends TestDimensions(nonSpecialDimensions)(gen) {

  def scalarSample = Iterator(0.0, 1.0) ++ (
    if(scalars != null) scalars.take(nonSpecialScalars) else Iterator.empty)

  def vectorSample(n:Int) =
    Iterator.continually(new Vector(elements.take(n).toArray))

  def generalMatrixSample(m:Int, n:Int): Iterator[Matrix] =
    Iterator.continually(new Matrix(m, n)(elements.take(m*n).toArray))

  def generalMatrixSample: Iterator[Matrix] =
    for {
      (m,n) <- twoDimensionSample
      a <- generalMatrixSample(m,n).take(matricesPerDimensions)
    } yield a

  def triangularMatrixSample(n:Int,
                             uplo:UpperOrLower.Value,
                             diag:DiagonalProperty.Value) =
    new Iterator[Matrix] {
      def hasNext = true

      def next = {
        val result = Matrix.zero(n)
        cforRange(0 until n) { i =>
          if(uplo == Upper)
            cforRange(i+1 until n) { j => result(i,j) = elements.next }
          else
            cforRange(0 until i) { j => result(i,j) = elements.next }
          if(diag == UnitDiagonal) result(i,i) = 1
          else result(i,i) = elements.next
        }
        result
      }
    }

  def triangularMatrixSample: Iterator[Matrix] = {
    for {
      n <- oneDimensionSample
      uplo <- Iterator(Upper, Lower)
      diag <- Iterator(UnitDiagonal, NonUnitDiagonal)
      a <- triangularMatrixSample(n, uplo, diag).take(matricesPerDimensions)
    } yield a
  }
}

trait CommonMatrixPropertyTests
extends BLAS.Level3 with NumericPropertiesOfDouble {

  val eps = precision

  def orthogonalityMeasure(q:MatrixLike) = {
    val (m,n) = q.dimensions
    val d = Matrix.identity(n)
    syrk(Lower, Transpose, -1.0, q, 1.0, d)
    d.norm1/(n*eps)
  }
}
