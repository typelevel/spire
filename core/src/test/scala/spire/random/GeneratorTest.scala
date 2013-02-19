package spire.random

import spire.syntax._

import org.scalatest.FunSuite

class GeneratorTest extends FunSuite {

  def getName(g: GeneratorCompanion[_, _]): String =
    g.getClass.getSimpleName.replace("$", "")

  /**
   * This is just a very basic sanity check.
   *
   * The idea is that for a large enough sample size (in this case
   * 10M) we expect the results of nextInt(mod) to be distributed
   * "evenly enough" across the 'mod' values.
   *
   * So, we build a histogram, fill it with 10M random values [0,
   * mod-1], then see how much each bucket's count deviates from the
   * expected count (which is 10M/mod).
   *
   * Since we're hardcoding the time (13572468L) to seed with, this
   * test doesn't cover all possible RNG states. You can uncomment the
   * line that uses a different seed each time to get better coverage,
   * but this may result in non-deterministic test failures (which
   * won't necessarily indicate a problem).
   *
   * For "real" RNG testing, a suite like DIEHARD is much more
   * appropriate than this file.
   */

  val size: Int = 10000000
  val threshold: Double = 0.003
  List(Lcg32, Lcg64, BurtleRot2, BurtleRot3, Marsaglia32a6, Cmwc5, Well512).foreach { gen => 
    val name = getName(gen)

    List(3, 5, 7, 11, 13, 17).foreach { mod =>
      test("%s nextInt(%d) distributed within %.1f%%" format (name, mod, threshold * 100)) {
        val histogram = new Array[Int](mod)
        //val rng = gen.fromTime()
        val rng = gen.fromTime(13572468L)
        cfor(0)(_ < size, _ + 1)(_ => histogram(rng.nextInt(mod)) += 1)
        val ratio = 1.0 * size / mod
        val deviation = histogram.toList.map(n => (1.0 - (n / ratio)).abs)
        assert(deviation.filter(_ > threshold) === Nil)
      }
    }
  }
}
