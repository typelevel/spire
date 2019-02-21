package spire
package benchmark
/*
import spire.math.prime._
import scala.util.{Success, Try}

object SieveBenchmark {

  def timer[A](s: String)(f: => A): A = {
    val t0 = System.nanoTime
    val a = f
    val t = System.nanoTime - t0
    println("%s generated %s in %.1f ms" format (s, a, t / 1000000.0))
    a
  }

  def main(args: Array[String]): Unit = {
    val defaults: List[Long] = (
      1L ::
      10L ::
      100L ::
      1L * 1000 ::
      10L * 1000 ::
      100L * 1000 ::
      1L * 1000 * 1000 ::
      10L * 1000 * 1000 ::
      // 100L * 1000 * 1000 ::
      // 1L * 1000 * 1000 * 1000 ::
      // 2L * 1000 * 1000 * 1000 ::
      // 4L * 1000 * 1000 * 1000 ::
      Nil
    )

    val ns = Try(args.toList.map(_.toLong).filter(_ > 0L)) match {
      case Success(ns) if ns.nonEmpty => ns
      case _ => defaults
    }

    println("warming up the sieves...")
    defaults.take(8).foreach(n => nth(n))
    println("done")

    System.gc()
    Thread.sleep(1000)

    println("timing the sieve...")
    ns.foreach(n => timer("  sieve.nth (%s)" format n)(nth(n)))
  }
}
*/