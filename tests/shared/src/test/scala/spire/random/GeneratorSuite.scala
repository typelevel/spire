/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package random

import spire.random.rng._
import spire.random.rng.extras._

class GeneratorSuite extends munit.FunSuite {

  /**
   * This is just a very basic sanity check.
   *
   * The idea is that for a large enough sample size (in this case 10M) we expect the results of nextInt(mod) to be
   * distributed "evenly enough" across the 'mod' values.
   *
   * So, we build a histogram, fill it with 10M random values [0, mod-1], then see how much each bucket's count deviates
   * from the expected count (which is 10M/mod).
   *
   * Since we're hardcoding the time (13572468L) to seed with, this test doesn't cover all possible RNG states. You can
   * uncomment the line that uses a different seed each time to get better coverage, but this may result in
   * non-deterministic test failures (which won't always even indicate a problem).
   *
   * For "real" RNG testing, a suite like DIEHARD is much more appropriate than this file.
   */

  def getName(g: GeneratorCompanion[_, _]): String =
    g.getClass.getSimpleName.replace("$", "")

  val size: Int = 10000000
  val threshold: Double = 0.0038

  // val bases: List[Int] = List(3, 5, 7, 11, 13, 17)
  val bases: List[Int] = Nil

  List(
    Lcg32,
    Lcg64,
    BurtleRot2,
    BurtleRot3,
    Marsaglia32a6,
    MersenneTwister32,
    MersenneTwister64,
    Cmwc5,
    Well512a,
    Well1024a,
    Well19937a,
    Well19937c,
    Well44497a,
    Well44497b,
    PcgXshRr64_32,
    XorShift64Star,
    XorShift1024Star,
    XorShift128Plus
  ).foreach { gen =>
    val name = getName(gen)

    test("sanity check") {
      bases.foreach { mod =>
        test("%s nextInt(%d) distributed within %.2f%%".format(name, mod, threshold * 100)) {
          val histogram = new Array[Int](mod)
          // val rng = gen.fromTime()
          val rng = gen.fromTime(13572468L)
          for (i <- 0 until size) {
            val n: Int = rng.nextInt(mod)
            histogram(n) += 1
          }
          val ratio = 1.0 * size / mod
          val deviation = histogram.toList.map(n => (1.0 - n / ratio).abs)
          assert(deviation.filter(_ > threshold).isEmpty)
        }
      }
    }
  }

  // def diagnostic {
  //   val nums = List(3, 5, 7, 11, 13, 17, 19, 23, 29, 31)

  //   def doit(name: String, mod: Int, f: Int => Int) {
  //     val histogram = new Array[Int](mod)
  //     for (i <- 0 until size) histogram(f(mod)) += 1
  //     val ratio = 1.0 * size / mod
  //     val deviation = histogram.toList.map(n => (1.0 - (n / ratio)).abs)
  //     println("%3d %.5f %-20s" format (mod, deviation.max, name))
  //   }

  //   val jrng = new java.util.Random()
  //   nums.foreach(mod => doit("Java48", mod, jrng.nextInt _))

  //   List(Lcg32, Lcg64, BurtleRot2, BurtleRot3, Marsaglia32a6, MersenneTwister32, MersenneTwister64, Cmwc5, Well512a, Well1024a, Well19937a, Well19937c, Well44497a, Well44497b).foreach { gen =>
  //     val name = getName(gen)
  //     val rng = gen.fromTime()
  //     nums.foreach(mod => doit(name, mod, rng.nextInt _))
  //   }
  // }
}
