package spire.benchmark

import ichi.bench.Thyme
import spire.implicits._

object MatrixMultiplicationBenchmarks {
  import spire.matrix.dense.Matrix
  import spire.matrix.dense.random.{
    Defaults,
    ScalarUniformDistributionFromMinusOneToOne
  }
  import spire.matrix.dense.BLAS
  object NaiveBlas3 extends BLAS.NaiveLevel3
  object FastBlas3 extends BLAS.FastLevel3

  import spire.matrix.dense.tests.RandomUncorrelatedElements

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
  val elts = new RandomUncorrelatedElements(elements=uniformIm1p1)

  def spireGemm(lvl3:BLAS.Level3, a:Matrix, b:Matrix, c:Matrix) = {
    import spire.matrix.Transposition._
    lvl3.gemm(NoTranspose, NoTranspose, 2.0, a, b, 0.0, c)
    val (m,n) = c.dimensions
    c
  }

  def jblasGemm(m:Int, n:Int, k:Int,
                a:Array[Double], b:Array[Double], c:Array[Double]) = {
    import org.jblas
    jblas.NativeBlas.dgemm('N', 'N',
                           m, n, k, 2.0, a, 0, m, b, 0, k,
                           0.0, c, 0, m)
    Matrix(m, n,  c)
  }

  def dimensions(minPowerOf2:Int, maxPowerOf2:Int) = for {
      Seq(k,l) <- (minPowerOf2 to maxPowerOf2).map(2**_).sliding(2)
      n <- Seq(k, (k+l)/2)
      dims <- Seq((n, n, n), (n, 2*n, n))
    } yield dims

  def main(args:Array[String]) {
    val (lo, hi) = if(args.size == 0) (1, 8)
                   else (args(0).toInt, args(1).toInt)
    println("Gflop/s for product of two n x n matrices")
    println("-----------------------------------------")
    println("%4s  %4s  %4s  %16s  %16s %16s".format(
            "m", "k", "n", "JBlas", "Spire (Naive)", "Spire (Fast)"))
    var delta = 0.0
    for((m,k,n) <- dimensions(lo, hi)) {
      val genA = elts.generalMatrixSample(m,k)
      val genB = elts.generalMatrixSample(k,n)
      val a = genA.next
      val b = genB.next
      val c = Matrix.empty(m, n)
      val timer = new Thyme
      val (c1, naiveReport) = timer.benchPair(spireGemm(NaiveBlas3, a, b, c))
      val aa = a.toArray
      val bb = b.toArray
      val cc = new Array[Double](m*n)
      val (c2, jblasReport) = timer.benchPair(jblasGemm(m, n, k, aa, bb, cc))
      val (c3, fastReport) = timer.benchPair(spireGemm(FastBlas3, a, b, c))
      val (u,v,w) = (c1(m/2, n/2), c2(m/2, n/2),c3(m/2, n/2))
      val deltaNaive = (u-v).abs/(u.abs + v.abs)
      val deltaFast = (u-w).abs/(u.abs + w.abs)
      delta += (deltaNaive + deltaFast)/2
      val flops = (2*k-1)*m*n
      val tm = naiveReport.runtime
      val (tl, th) = naiveReport.runtimeCI95
      val jtm = jblasReport.runtime
      val (jtl, jth) = jblasReport.runtimeCI95
      val ftm = fastReport.runtime
      val (ftl, fth) = fastReport.runtimeCI95
      Seq(tm, tl, th, jtm, jtl, jth, ftm, ftl, fth).map(flops/1e9/_) match {
        case Seq(gm, gh, gl, jgm, jgh, jgl, fgm, fgh, fgl)
          => println(("%4d  %4d  %4d  " ++ "%4.2f +%4.2f -%4.2f  "*3).
                      format(m, k, n,
                             jgm, jgh-jgm, jgm-jgl,
                             gm, gh-gm, gm-gl,
                             fgm, fgh-fgm, fgm-fgl))
      }
    }
    println("Error: %.2f %%".format(delta/100))
  }
}
