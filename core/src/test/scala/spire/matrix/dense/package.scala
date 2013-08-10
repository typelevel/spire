package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.BLAS
import BLAS.Transposition._
import BLAS.UpperOrLower._
import spire.matrix.NumericPropertiesOfDouble

/**
 */


/**
 * Dimension sampling
 */
class TestDimensions(nonSpecialDimensions:Int=0)
                    (implicit gen:Defaults.IntegerGenerator) {

  def oneDimensionSample =
    List(1, 2, 3, 5, 10, 16) ++
    List.fill(nonSpecialDimensions)(gen.nextInt(32, 64))

  def twoDimensionSample =
    for(m <- oneDimensionSample; n <- oneDimensionSample) yield (m,n)
}

/**
 * Test general matrices, mostly to test the like of BLAS
 */
class TestGeneralMatrices(nonSpecialDimensions:Int=0,
                          nonSpecialScalars:Int=1,
                          matricesPerDimensions:Int=1,
                          elements:ScalarDistribution,
                          scalars:ScalarDistribution=null)
                         (implicit gen:Defaults.IntegerGenerator)
extends TestDimensions(nonSpecialDimensions)(gen) {

  def scalarSample = List(0.0, 1.0) ++ (
    if(scalars != null) scalars.take(nonSpecialScalars).toList else Nil)

  def generalMatrixSample(m:Int, n:Int): Iterator[Matrix] =
    Iterator.continually(new Matrix(m, n)(elements.take(m*n).toArray))

  def generalMatrixSample: Iterator[Matrix] =
    for {
      (m,n) <- twoDimensionSample.iterator
      a <- generalMatrixSample(m,n).take(matricesPerDimensions)
    } yield a
}
