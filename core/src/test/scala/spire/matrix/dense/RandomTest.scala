package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.Transposition._
import spire.matrix.UpperOrLower._
import spire.matrix.BLAS
import BLAS.level3
import spire.random.{Well512,GaussianGenerator}
import spire.matrix.Sides._
import spire.matrix.dense.random._

import jdistlib.disttest.NormalityTest.{dagostino_pearson_statistic,
                                        dagostino_pearson_pvalue}
import jdistlib.disttest.DistributionTest
import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt

import org.scalatest.FunSuite

class RandomOrthogonalMatrixTest extends FunSuite
with CommonMatrixPropertyTests
with BLAS.level3.Naive
{
  implicit val gen = Defaults.IntegerGenerator.fromTime(3)

  test("Orthogonality") {
    val dims = new TestDimensions(nonSpecialDimensions=5)
    implicit val scratchpad = new Scratchpad(
      OrthogonalMatricesHaarDistribution.minimumScratchpad(
        dims.oneDimensionSample.max))
    for(n <- dims.oneDimensionSample;
        haar = new OrthogonalMatricesHaarDistribution(n))
    {
      val q = Matrix.identity(n)
      haar.overwriteWithProductByNext(fromLeft, q)
      haar.overwriteWithProductByNext(fromRight, q)
      haar.overwriteWithProductByNext(Congruent, q)
      val orth = orthogonalityMeasure(q)
      assert(orth < 20)
    }
  }

  test("Haar distribution") {
    /*
      According to [1, Theorem 3.7], M being drawn from the Haar measure
      over the set of orthogonal matrices, the random variable

        (Tr M^k - eta(k))/sqrt(k),

      where eta(k) = 1 if k is even, and 0 otherwise, is asymptotically
      distributed as a standard normal distribution as the dimension of n
      increases (the convergence proceeds at an exponential rate).

      Thus we are going to test that

        Tr M  ~  (Tr M^2 - 1)/sqrt(2)  ~  (Tr M^3)/sqrt(3)  ~  N(0,1)
    */
    val n = 16
    implicit val work = new Scratchpad(
      OrthogonalMatricesHaarDistribution.minimumScratchpad(n))
    val sampleCnt = 100
    val haar = new OrthogonalMatricesHaarDistribution(n)
    val rndVars  = for(k <- 1 to 3) yield new ArrayBuffer[Double]
    for(cnt <- 1 to sampleCnt) {
      val m = haar.next
      rndVars(0) += m.trace
      val m2 = Matrix.empty(n, n)
      gemm(NoTranspose, NoTranspose, 1.0, m, m, 0.0, m2)
      rndVars(1) += (m2.trace - 1)/sqrt(2.0)
      val m3 = Matrix.empty(n, n)
      gemm(NoTranspose, NoTranspose, 1.0, m, m2, 0.0, m3)
      rndVars(2) += m3.trace/sqrt(3.0)
    }
    val samples = for(rndVar <- rndVars) yield rndVar.toArray
    for {
      (k, sample) <- (1 to 3) zip samples
      kSq = dagostino_pearson_statistic(sample)
      p = dagostino_pearson_pvalue(kSq)
    } {
      assert(p > 0.005,
             """|Tr M^%d statistics deviates from normality
                | at probability level %.3g""".format(k, p).stripMargin)
    }
    val p12 = DistributionTest.kolmogorov_smirnov_pvalue(
      DistributionTest.kolmogorov_smirnov_statistic(samples(0), samples(1)),
      sampleCnt, sampleCnt)
    val p23 = DistributionTest.kolmogorov_smirnov_pvalue(
      DistributionTest.kolmogorov_smirnov_statistic(samples(1), samples(2)),
      sampleCnt, sampleCnt)
    assert(p12 > 0.005,
           "Tr M incompatible with Tr M^2 at level %.3g".format(p12))
    assert(p23 > 0.005,
           "Tr M^2 incompatible with Tr M^3 at level %.3g".format(p23))
  }
}
