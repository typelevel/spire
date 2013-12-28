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
    Iterator.continually(Vector.fill(n)(elements.next))

  def generalMatrixSample(m:Int, n:Int): Iterator[Matrix] =
    Iterator.continually(Matrix.fill(m, n)(elements.next))

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

  def matrixProductSample = {
    val shapes = for {
      transA <- Iterator(NoTranspose, Transpose)
      transB <- Iterator(NoTranspose, Transpose)
      m0 <- oneDimensionSample
      k0 <- oneDimensionSample
      n0 <- oneDimensionSample
      (m,k,n) <- Seq((m0,k0,n0),
                     (m0+m0/3,k0,n0), (m0,k0+k0/3,n0), (m0,k0,n0+n0/3)).distinct
      flops = m*k*n
    } yield (flops, transA, transB, m, k, n)
    def generateOneMatrix(mn:(Int,Int)) =
      generalMatrixSample(mn._1, mn._2).take(1)
    for {
      (flops, transA, transB, m, k, n) <- shapes.toSeq.sortWith(_._1 < _._1)
      dimA = if(transA == NoTranspose) (m,k) else (k,m)
      dimB = if(transB == NoTranspose) (k,n) else (n,k)
      a <- generateOneMatrix(dimA)
      b <- generateOneMatrix(dimB)
      alpha <- scalarSample
      beta <- scalarSample
    } yield (transA, transB, alpha, a, b, beta, m, n, k)
  }
}

trait CommonMatrixPropertyTests
extends BLAS.Level3 with NumericPropertiesOfDouble {

  val eps = precision

  def orthogonalityMeasure(q:Matrix) = {
    val (m,n) = q.dimensions
    val d = Matrix.identity(n)
    syrk(Lower, Transpose, -1.0, q, 1.0, d)
    d.norm1/(n*eps)
  }
}
