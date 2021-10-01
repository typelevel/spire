/**
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
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
package rng

import spire.syntax.fastFor._
import spire.math.max

/**
 * This object provides helper functions used for seeding arrays of integers or longs.
 *
 * The seeding functions are an adaptation/port of code from the the 32-bit and 64-bit implementations of MersenneTwister (MT19937.c, MT19937-64.c).
 *
 * <p>MersenneTwister is a fast, 623-dimensionally equidistributed pseudo random number generator
 * with a <tt>2<sup>19937</sup>&nbsp;-&nbsp;1</tt> long period.
 *
 * <p><b>Reference: </b>
 * Makato Matsumoto and Takuji Nishimura:
 * "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform Pseudo-Random Number Generator",
 * <i>ACM Transactions on Modeling and Computer Simulation,</i> Vol. 8, No. 1, January 1998, pp 3--30.
 *
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c">MT19937.c</a>
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/mt19937-64.c">MT19937-64.c</a>
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">Mersenne Twister Home Page</a>
 * @see <a href="http://en.wikipedia.org/wiki/Mersenne_twister">Mersenne Twister @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
object Utils {
  /*
    final class IntArrayWrapper(transform: Int => Int, array: Array[Int]) {
      def apply(i: Int) = array(transform(i))
      def update(i: Int, v: Int) = array(transform(i)) = v
    }

    final class LongArrayWrapper(transform: Int => Int, array: Array[Long]) {
      def apply(i: Int) = array(transform(i))
      def update(i: Int, v: Int) = array(transform(i)) = v
   }
   */

  @volatile private var seedUniquifier = 8682522807148012L

  def intFromTime(time: Long = System.nanoTime): Int = {
    longFromTime(time).toInt
  }

  def longFromTime(time: Long = System.nanoTime): Long = {
    seedUniquifier += 1
    (seedUniquifier + time)
  }

  def seedFromInt(length: Int, seed: Int = 5489): Array[Int] = {
    val a = new Array[Int](length)
    a(0) = seed

    fastFor(1)(_ < length, _ + 1) { i =>
      val x = a(i - 1)
      a(i) = 1812433253 * (x ^ (x >>> 30)) + i
    }

    a
  }

  def seedFromLong(length: Int, seed: Long = 5489): Array[Long] = {
    val a = new Array[Long](length)
    a(0) = seed

    fastFor(1)(_ < length, _ + 1) { i =>
      val x = a(i - 1)
      a(i) = 6364136223846793005L * (x ^ (x >>> 62)) + i
    }

    a
  }

  def seedFromArray(length: Int, seed: Array[Int]): Array[Int] = {
    val a = seedFromInt(length, 19650218)
    val length_1 = length - 1

    var i = 1
    var j = 0
    var k = max(length, seed.length)

    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 30)) * 1664525) + seed(j) + j
      i += 1
      j += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      if (j >= seed.length) {
        j = 0
      }
      k -= 1
    }

    k = length_1
    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 30)) * 1566083941) - i
      i += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      k -= 1
    }

    a(0) = 0x80000000 // MSB is 1; assuring non-zero initial array
    a
  }

  def seedFromArray(length: Int, seed: Array[Long]): Array[Long] = {
    val a = seedFromLong(length, 19650218)
    val length_1 = length - 1

    var i = 1
    var j = 0
    var k = max(length, seed.length)

    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 62)) * 3935559000370003845L) + seed(j) + j
      i += 1
      j += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      if (j >= seed.length) {
        j = 0
      }
      k -= 1
    }

    k = length - 1
    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 62)) * 2862933555777941757L) - i
      i += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      k -= 1
    }

    a(0) = 1L << 63 // MSB is 1; assuring non-zero initial array
    a
  }
}
