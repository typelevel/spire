package spire.benchmark

import spire.matrix.dense.Matrix
import spire.matrix.dense.random.{
  Defaults,
  ScalarUniformDistributionFromMinusOneToOne
}
import spire.matrix.dense.BLAS
object NaiveBlas3 extends BLAS.NaiveLevel3
object FastBlas3 extends BLAS.FastLevel3

import spire.matrix.dense.tests.RandomUncorrelatedElements

import ichi.bench.Thyme
import spire.implicits._

object BLAS3Bench {

  def discrepancy(ref:Matrix, others:Matrix*): Double = {
    val tRef = ref.trace
    others.map((a:Matrix) => {
      val t = a.trace
      (t - tRef).abs/(t.abs + tRef.abs)
    }).sum/others.size
  }

  def display(flops:Double, reports:Thyme.Benched*): String = {
    val gflops = flops/1e9
    reports.map((r:Thyme.Benched) => {
      val tm = r.runtime
      val (tl, th) = r.runtimeCI95
      val (gm, gl, gh) = (gflops/tm, gflops/th, gflops/tl)
      "%4.2f +%4.2f -%4.2f".format(gm, gh-gm, gm-gl)
    }).mkString("  ")
  }
}

object MatrixMultiplicationBenchmarks {
  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
  val elts = new RandomUncorrelatedElements(elements=uniformIm1p1)

  def spireGemm(lvl3:BLAS.Level3, a:Matrix, b:Matrix, c:Matrix) = {
    import spire.matrix.Transposition._
    lvl3.gemm(NoTranspose, NoTranspose, 2.0, a, b, 1.0, c)
    val (m,n) = c.dimensions
    c
  }

  def jblasGemm(m:Int, n:Int, k:Int,
                a:Array[Double], b:Array[Double], c:Array[Double]) = {
    import org.jblas
    jblas.NativeBlas.dgemm('N', 'N',
                           m, n, k, 2.0, a, 0, m, b, 0, k,
                           1.0, c, 0, m)
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
    println("%4s  %4s  %4s  %16s  %16s  %16s".format(
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
      delta += BLAS3Bench.discrepancy(c2, c1, c3)
      val flops = (2*k-1)*m*n
      println("%4d  %4d  %4d  ".format(m, k, n) ++
              BLAS3Bench.display(flops, jblasReport, naiveReport, fastReport))
    }
    println("Error: %.2f %%".format(delta*100))
  }
}

object TriangularSolverBenchmarks {
  import spire.matrix.{Sides, UpperOrLower, Transposition, DiagonalProperty}
  import Sides._
  import UpperOrLower._
  import Transposition._
  import DiagonalProperty._

  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
  val elts = new RandomUncorrelatedElements(elements=uniformIm1p1)

  def spireTrsm(lvl3:BLAS.Level3, a:Matrix, b:Matrix) = {
    lvl3.trsm(FromLeft, Lower, NoTranspose, UnitDiagonal, 1.0, a, b)
    val (m,n) = b.dimensions
    b
  }

  def dimensions(minPowerOf2:Int, maxPowerOf2:Int, reverse:Boolean) = for {
      Seq(k,l) <- (if(reverse) maxPowerOf2 to minPowerOf2 by -1
                   else minPowerOf2 to maxPowerOf2).map(2**_).sliding(2)
      m <- Seq(k, (k+l)/2)
    } yield m

  def main(args:Array[String]) {
    val (lo, hi) = if(args.size == 0) (1, 8)
                   else (args(0).toInt, args(1).toInt)
    val reverse = args.size == 3 && args(2) == "R"
    println(s">${reverse}<")
    println("Gflop/s for triangular solver A X = B with all matrices m x m")
    println("-------------------------------------------------------------")
    println("%4s  %16s  %16s".format(
            "m", "Spire (Naive)", "Spire (Fast)"))
    var delta = 0.0
    for(m <- dimensions(lo, hi, reverse)) {
      val genA = elts.triangularMatrixSample(m, Lower, UnitDiagonal)
      val genB = elts.generalMatrixSample(m,m)
      val a = genA.next
      val b = genB.next
      val timer = new Thyme
      val b1 = b.copyToMatrix
      val (x1, naiveReport) = timer.benchPair(spireTrsm(NaiveBlas3, a, b1))
      val b2 = b.copyToMatrix
      val (x2, fastReport) = timer.benchPair(spireTrsm(FastBlas3, a, b2))
      delta += BLAS3Bench.discrepancy(x1, x2)
      val flops = m*(m-1) * m
      println("%4d  ".format(m) ++
              BLAS3Bench.display(flops, naiveReport, fastReport))
    }
    println("Error: %.2f %%".format(delta/100))
  }
}
