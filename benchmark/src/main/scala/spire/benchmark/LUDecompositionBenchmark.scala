package spire.benchmark

import spire.matrix.dense._
import spire.matrix.dense.random.{
  Defaults,
  ScalarUniformDistributionFromMinusOneToOne
}
import spire.matrix.dense.BLAS
import BLAS._
import spire.matrix.Transposition._

import spire.matrix.dense.tests.{LinearSystemTestMatrices,
                                 RandomUncorrelatedElements}

import spire.implicits._

import org.jblas
import com.github.fommil.netlib
import org.netlib.util

trait LUDecompositionBenchmark extends MatrixBenchmark {
  type Arguments = Matrix

  def trashCaches(a:Arguments) {
    val (m,n) = a.dimensions
    a(0,0) = a(m-1,m-1) + a(m/2,m/2)
    a(m/2,n/2) += 2*a(0,0)
    a(m-1, n-1) = a(0,0) - a(m/2,m/2)
  }

  def flops(a:Arguments) = {
    val m = a.dimensions._1.toLong
    m*m*m
  }
}

class JBlasLUDecompositionBenchmark extends LUDecompositionBenchmark {
  def header = "jblas"
  def benched(a:Arguments) = {
    val m = a.dimensions._1
    val ipiv = new Array[Int](m)
    jblas.NativeBlas.dgetrf(m, m, a.elements, 0, m, ipiv, 0)
    a
  }
}

class NetlibJavaLUDecompositionBenchmark extends LUDecompositionBenchmark {
  def header = "netlib-java"
  val lapack = netlib.LAPACK.getInstance()
  def benched(a:Arguments) = {
    val m = a.dimensions._1
    val ipiv = new Array[Int](m)
    val info = new org.netlib.util.intW(0)
    lapack.dgetrf(m, m, a.elements, m, ipiv, info)
    a
  }
}

trait SpireLUDecompositionConstructionForBenchmark
extends LU.DecompositionConstruction
with BLAS.LayeredLevel3 with BLAS.NaiveLevel2 with BLAS.NaiveLevel1 {
  def raw(lu1:Matrix, p1:Permutation) =
    new LU.Decomposition
    with BLAS.LayeredLevel3 with BLAS.NaiveLevel2 with BLAS.NaiveLevel1 {
      val lu = lu1
      val p = p1
    }
}

trait SpireLUDecompositionBenchmark extends LUDecompositionBenchmark {
  val LUDecomposition: SpireLUDecompositionConstructionForBenchmark
  def benched(a:Arguments) = {
    LUDecomposition(a)
    a
  }
}

trait SpireRecursiveLUDecompositionBenchmark
extends SpireLUDecompositionBenchmark {
  def blockingSize: Int
  def header = "recursive (%d)".format(blockingSize)

  object LUDecomposition
  extends SpireLUDecompositionConstructionForBenchmark
  with LU.RecursiveDecompositionConstruction {
    val unblockedThreshold = blockingSize
  }

}

trait SpireClassicBlockedLUDecompositionBenchmark
extends SpireLUDecompositionBenchmark {
  val blockSize:Int
  def header = "classic (%d)".format(blockSize)

  object LUDecomposition
  extends SpireLUDecompositionConstructionForBenchmark
  with LU.ClassicLeftToRightBlockedDecompositionConstruction {
    val nb = blockSize
  }
}

trait SpireFusedBlockedLUDecompositionBenchmark
extends SpireLUDecompositionBenchmark {
  val blockSize:Int
  def header = "fused (%d)".format(blockSize)

  object LUDecomposition
  extends SpireLUDecompositionConstructionForBenchmark
  with LU.FusedLeftToRightBlockedDecompositionConstruction {
    val nb = blockSize
    lazy val blocking = GEBP.blocking.get()
  }
}


object LUDecompositionBenchmarks {
  implicit val gen = Defaults.IntegerGenerator.fromTime(System.nanoTime)
  val matrices = new LinearSystemTestMatrices(includedTypes = Set(4))

  def dimensions(minPowerOf2:Int, maxPowerOf2:Int) = for {
      Seq(k,l) <- (minPowerOf2 to maxPowerOf2).map(2**_).sliding(2)
      m <- Seq(k, (k+l)/2)
    } yield m

  def main(args:Array[String]) {
    val (lo, hi) = if(args.size == 0) (1, 8)
                   else (args(0).toInt, args(1).toInt)
    println("Gflops/s for LU Decomposition of m x m matrix")
    println("---------------------------------------------")
    val dimsHeader = "%4s".format("m")
    val benchmarks = new JBlasLUDecompositionBenchmark ::
                     //new NetlibJavaLUDecompositionBenchmark ::
                     new SpireRecursiveLUDecompositionBenchmark {
                       val blockingSize = 4
                     } ::
                     new SpireRecursiveLUDecompositionBenchmark {
                       val blockingSize = 16
                     } ::
                     new SpireRecursiveLUDecompositionBenchmark {
                       val blockingSize = 32
                     } ::
                     new SpireRecursiveLUDecompositionBenchmark {
                       val blockingSize = 64
                     } ::
                     new SpireClassicBlockedLUDecompositionBenchmark {
                       val blockSize = 16
                     } ::
                     new SpireClassicBlockedLUDecompositionBenchmark {
                       val blockSize = 32
                     } ::
                     new SpireClassicBlockedLUDecompositionBenchmark {
                       val blockSize = 64
                     } ::
                     new SpireFusedBlockedLUDecompositionBenchmark {
                       val blockSize = 16
                     } ::
                     new SpireFusedBlockedLUDecompositionBenchmark {
                       val blockSize = 32
                     } ::
                     new SpireFusedBlockedLUDecompositionBenchmark {
                       val blockSize = 64
                     } ::
                     Nil
    val benchmarksHeader = benchmarks.map(_.formatedHeader).mkString("  ")
    println(dimsHeader ++ "  " ++ benchmarksHeader)

    implicit val timer = new MatrixTimer

    for {
      m <- dimensions(lo, hi)
      (_, _, a) <- matrices.sample(m,m)
      a1 = a.copyToMatrix
    } {
      println("%4d".format(m) ++ "  " ++
              benchmarks.map(_.formatedTiming(a)).mkString("  "))
    }
  }
}
