/************************************************************************\
** Project                                                              **
**       ______  ______   __    ______    ____                          **
**      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
**     / /__   / /_/ /  / /   / /_/ /   / /_                            **
**    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
**   ____/ / / /      / /   / / | |   / /__                             **
**  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
**                                                                      **
**      Redistribution and use permitted under the MIT license.         **
**                                                                      **
\************************************************************************/


package spire
package random
package rng

import spire.syntax.cfor._
import spire.util.Pack
import java.nio.ByteBuffer
import java.util.Arrays

/**
 * This is a 32-bit Scala implementation of MersenneTwister based on MT19937.c.
 *
 * <p>MersenneTwister is a fast, 623-dimensionally equidistributed pseudo random number generator
 * with a <tt>2<sup>19937</sup>&nbsp;-&nbsp;1</tt> long period.
 *
 * <p><b>Reference: </b>
 * Makoto Matsumoto and Takuji Nishimura:
 * "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform Pseudo-Random Number Generator",
 * <i>ACM Transactions on Modeling and Computer Simulation,</i> Vol. 8, No. 1, January 1998, pp 3--30.
 *
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c">MT19937.c</a>
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">Mersenne Twister Home Page</a>
 * @see <a href="http://en.wikipedia.org/wiki/Mersenne_twister">Mersenne Twister @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
final class MersenneTwister32 protected[random](mt: Array[Int], mti0: Int = 625) extends IntBasedGenerator { // N + 1 == 625

  import MersenneTwister32.{UpperMask, LowerMask, N, M, N_M, N_1, M_N, M_1, BYTES, mag01}

  private var mti = mti0

  def copyInit: MersenneTwister32 = new MersenneTwister32(mt.clone, mti)

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    cfor(0)(_ < N, _ + 1) { i => bb.putInt(mt(i)) }
    bb.putInt(mti)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    cfor(0)(_ < N, _ + 1) { i => mt(i) = bb.getInt() }
    mti = bb.getInt
  }

  // Generates the next random integer in the sequence
  def nextInt(): Int = {
    var y = 0

    if (mti >= N) {
      var kk = 0

      while (kk < N_M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y)
        kk += 1
      }

      while (kk < N_1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M_N)) ^ (y >>> 1) ^ mag01(y)
        kk += 1
      }

      y = (mt(N_1) & UpperMask) | (mt(0) & LowerMask)
      mt(N_1) = mt(M_1) ^ (y >>> 1) ^ mag01(y)

      mti = 0
    }

    y = mt(mti)
    mti += 1

    // Tempering
    y ^= (y >>> 11)
    y ^= (y <<   7) & 0x9D2C5680
    y ^= (y <<  15) & 0xEFC60000
    y ^= (y >>> 18)

    y
  }
}

object MersenneTwister32 extends GeneratorCompanion[MersenneTwister32, (Array[Int], Int)] {

  @inline private val UpperMask = 0x80000000 // = Int.MinValue = 0xFFFFFFFF ^ Int.MaxValue
  @inline private val LowerMask = 0x7FFFFFFF // = Int.MaxValue = 0xFFFFFFFF ^ Int.MinValue

  @inline private val N = 624
  @inline private val M = 397

  @inline private val N_M = N - M
  @inline private val N_1 = N - 1

  @inline private val M_N = M - N
  @inline private val M_1 = M - 1

  @inline private val BYTES = N * 4 + 4

  @inline private def mag01(x: Int) = if((x & 1) == 0) 0 else 0x9908B0DF

  def randomSeed(): (Array[Int], Int) = (Utils.seedFromInt(N, Utils.intFromTime()), N + 1)

  def fromSeed(seed: (Array[Int], Int)): MersenneTwister32 =
    seed match {
      case (mt, mti) =>
        assert(mt.length == N)
        new MersenneTwister32(mt, mti)
    }

  def fromArray(arr: Array[Int]): MersenneTwister32 =
    fromSeed((Utils.seedFromArray(N, arr), N + 1))

  def fromBytes(bytes: Array[Byte]): MersenneTwister32 =
    fromArray(Pack.intsFromBytes(bytes, bytes.length / 4))

  def fromTime(time: Long = System.nanoTime) : MersenneTwister32 =
    fromSeed((Utils.seedFromInt(N, Utils.intFromTime(time)), N + 1))
}
