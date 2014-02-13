package spire.benchmark

import spire.matrix.dense.Matrix
import spire.matrix.dense.random.{
  Defaults,
  ScalarUniformDistributionFromMinusOneToOne
}
import spire.matrix.dense.BLAS
import BLAS._
import spire.matrix.{Sides, UpperOrLower, Transposition, DiagonalProperty}
import Sides._
import UpperOrLower._
import Transposition._
import DiagonalProperty._

import spire.matrix.dense.tests.RandomUncorrelatedElements

import ichi.bench.Thyme
import spire.implicits._

import org.jblas

trait MatrixBenchmark {
  def header:String
  type Arguments
  def benched(args:Arguments): Matrix
  def gflops(args:Arguments): Double

  def formatTitle(h:String) = "%17s".format(h)
  def formatedHeader = formatTitle(header)

  def formatedTiming(args:Arguments)(implicit timer:Thyme) = {
    val (result, report) = timer.benchPair(benched(args))
    val tm = report.runtime
    val (tl, th) = report.runtimeCI95
    val g = gflops(args)/1e9
    val (gm, gl, gh) = (g/tm, g/th, g/tl)
    val (mean, plusSigma, minusSigma) = (gm, gh-gm, gm-gl)
    "%5.2f +%4.2f -%4.2f".format(mean, plusSigma, minusSigma)
  }
}

trait GemmBenchmark extends MatrixBenchmark {
  type Arguments = (Matrix, Matrix, Matrix)
  def gflops(args:Arguments) = {
    val (a, b, c) = args
    val (m,n) = c.dimensions
    val k = a.dimensions._2
    (2*k-1)*m*n
  }
}

trait SpireGemmBenchmark extends GemmBenchmark {
  val lvl3:BLAS.Level3
  def benched(args:Arguments) = {
    val (a, b, c) = args
    lvl3.gemm(NoTranspose, NoTranspose, 2.0, a, b, 1.0, c)
    c
  }
}

trait SpireNaiveBenchmark extends MatrixBenchmark {
  def header = "spire (naive)"
  val lvl3 = BLAS.NaiveLevel3
}

trait SpireLayeredBenchmark extends MatrixBenchmark {
  def header = "spire (layered)"
  val lvl3 = BLAS.LayeredLevel3
}

class JBlasGemmBenchmark extends GemmBenchmark {
  val header = "jblas"
  def benched(args:Arguments) = {
    val (a, b, c) = args
    val (m,n) = c.dimensions
    val k = a.dimensions._2
    jblas.NativeBlas.dgemm('N', 'N',
                           m, n, k, 2.0, a.elements, 0, m, b.elements, 0, k,
                           1.0, c.elements, 0, m)
    c
  }
}

trait BenchmarkGenerator {
  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  val uniform = new ScalarUniformDistributionFromMinusOneToOne
  val elts = new RandomUncorrelatedElements(elements=uniform)
}

object MatrixMultiplicationBenchmarks extends BenchmarkGenerator {

  def dimensions(minPowerOf2:Int, maxPowerOf2:Int) = for {
      Seq(k,l) <- (minPowerOf2 to maxPowerOf2).map(2**_).sliding(2)
      n <- Seq(k, (k+l)/2)
      dims <- Seq((n, n, n), (n, 2*n, n))
    } yield dims

  def main(args:Array[String]) {
    val (lo, hi) = if(args.size == 0) (1, 8)
                   else (args(0).toInt, args(1).toInt)
    println("Gflops/s for product of two general matrices")
    println("--------------------------------------------")
    val dimsHeader = "%4s  %4s  %4s".format("m", "k", "n")
    val benchmarks = new JBlasGemmBenchmark ::
                     new SpireGemmBenchmark with SpireNaiveBenchmark ::
                     new SpireGemmBenchmark with SpireLayeredBenchmark ::
                     Nil
    val benchmarksHeader = benchmarks.map(_.formatedHeader).mkString("  ")
    println(dimsHeader ++ "  " ++ benchmarksHeader)

    implicit val timer = new Thyme

    for {
      (m,k,n) <- dimensions(lo, hi)
      a <- elts.generalMatrixSample(m,k).take(1)
      b <- elts.generalMatrixSample(k,n).take(1)
      c = Matrix.empty(m, n)
    } {
      println("%4d  %4d  %4d".format(m,k,n) ++ "  " ++
              benchmarks.map(_.formatedTiming((a, b, c))).mkString("  "))
    }
  }
}

trait TrsmBenchmark extends MatrixBenchmark {
  type Arguments = (Matrix, Matrix)
  def topHeader:String
  def formattedTopHeader = formatTitle(topHeader)
  val side: Sides.Value
  val uplo: UpperOrLower.Value
  val trans: Transposition.Value
  val diag: DiagonalProperty.Value

  def gflops(args:Arguments) = {
    val (a, b) = args
    val (m,n) = b.dimensions
    val (p,q) = if(side == FromLeft) (m,n) else (n,m)
    p*(p+1)*q
  }
}

trait Benchmark_L_X_eq_B extends TrsmBenchmark {
  def topHeader = "L X = B"
  val side  = FromLeft
  val uplo  = Lower
  val trans = NoTranspose
  val diag  = NonUnitDiagonal
}

trait Benchmark_X_U_eq_B extends TrsmBenchmark {
  def topHeader = "X U = B"
  val side  = FromRight
  val uplo  = Upper
  val trans = NoTranspose
  val diag  = NonUnitDiagonal
}

trait SpireTrsmBenchmark extends TrsmBenchmark {
  val lvl3: BLAS.Level3
  def benched(args:Arguments) = {
    val (a, b) = args
    lvl3.trsm(side, uplo, trans, diag, 2.0, a, b)
    b
  }
}

object TriangularSolverBenchmarks extends BenchmarkGenerator {

  def dimensions(minPowerOf2:Int, maxPowerOf2:Int, reverse:Boolean) = for {
      Seq(k,l) <- (if(reverse) maxPowerOf2 to minPowerOf2 by -1
                   else minPowerOf2 to maxPowerOf2).map(2**_).sliding(2)
      m <- Seq(k, (k+l)/2)
    } yield m

  def main(args:Array[String]) {
    val (lo, hi) = if(args.size == 0) (1, 8)
                   else (args(0).toInt, args(1).toInt)
    val reverse = args.size == 3 && args(2) == "R"
    println("Gflop/s for triangular solver A X = B with all matrices m x m")
    println("-------------------------------------------------------------")
    val dimsHeader = "%4s".format("m")
    val benchmarks =
      new Benchmark_L_X_eq_B with SpireTrsmBenchmark with SpireNaiveBenchmark ::
      new Benchmark_L_X_eq_B with SpireTrsmBenchmark with SpireLayeredBenchmark ::
      new Benchmark_X_U_eq_B with SpireTrsmBenchmark with SpireNaiveBenchmark ::
      new Benchmark_X_U_eq_B with SpireTrsmBenchmark with SpireLayeredBenchmark ::
      Nil

    val benchmarksTopHeader = benchmarks.map(_.formattedTopHeader).mkString("  ")
    val benchmarksHeader = benchmarks.map(_.formatedHeader).mkString("  ")
    println("    "     ++ "  " ++ benchmarksTopHeader)
    println(dimsHeader ++ "  " ++ benchmarksHeader)

    implicit val timer = new Thyme

    for {
      m <- dimensions(lo, hi, reverse)
      a <- elts.triangularMatrixSample(m, Lower, UnitDiagonal).take(1)
      b <- elts.generalMatrixSample(m,m).take(1)
    } {
      println("%4d  ".format(m) ++ "  " ++
              benchmarks.map(_.formatedTiming((a, b))).mkString("  "))
    }
  }
}

