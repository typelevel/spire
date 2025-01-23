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
package rng

import spire.syntax.cfor._
import spire.util.Pack
import java.nio.ByteBuffer
import java.util.Arrays

/**
 * This is a 64-bit Scala implementation of MersenneTwister based on MT19937-64.c.
 *
 * <p>MersenneTwister is a fast, 623-dimensionally equidistributed pseudo random number generator with a
 * <tt>2<sup>19937</sup>&nbsp;-&nbsp;1</tt> long period.
 *
 * <p><b>Reference: </b> Makoto Matsumoto and Takuji Nishimura: "Mersenne Twister: A 623-Dimensionally Equidistributed
 * Uniform Pseudo-Random Number Generator", <i>ACM Transactions on Modeling and Computer Simulation,</i> Vol. 8, No. 1,
 * January 1998, pp 3--30.
 *
 * @see
 *   <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/mt19937-64.c">MT19937-64.c</a>
 * @see
 *   <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">Mersenne Twister Home Page</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Mersenne_twister">Mersenne Twister @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
final class MersenneTwister64 protected[random] (mt: Array[Long], mti0: Int = 313)
    extends LongBasedGenerator { // N + 1 = 313

  import MersenneTwister64.{mag01, BYTES, LowerMask, M, M_1, M_N, N, N_1, N_M, UpperMask}

  private var mti = mti0

  def copyInit: MersenneTwister64 = new MersenneTwister64(mt.clone, mti)

  def getSeedBytes: Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    cfor(0)(_ < N, _ + 1) { i => bb.putLong(mt(i)) }
    bb.putInt(mti)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    cfor(0)(_ < N, _ + 1) { i => mt(i) = bb.getLong }
    mti = bb.getInt
  }

  // Generates the next random long in the sequence
  override def nextLong(): Long = {
    var x = 0L

    if (mti >= N) {
      var kk = 0

      while (kk < N_M) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      while (kk < N_1) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M_N) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      x = (mt(N_1) & UpperMask) | (mt(0) & LowerMask)
      mt(N_1) = mt(M_1) ^ (x >>> 1) ^ mag01(x)

      mti = 0
    }

    x = mt(mti)
    mti += 1

    // Tempering
    x ^= (x >>> 29) & 0x5555555555555555L
    x ^= (x << 17) & 0x71d67fffeda60000L
    x ^= (x << 37) & 0xfff7eee000000000L
    x ^= (x >>> 43)

    x
  }
}

object MersenneTwister64 extends GeneratorCompanion[MersenneTwister64, (Array[Long], Int)] {

  @inline private val UpperMask = 0xffffffff80000000L // = 0xFFFFFFFFFFFFFFFFL ^ Int.MinValue
  @inline private val LowerMask = 0x7fffffffL // = Int.MinValue

  @inline private val N = 312
  @inline private val M = 156

  @inline private val N_M = N - M
  @inline private val N_1 = N - 1

  @inline private val M_N = M - N
  @inline private val M_1 = M - 1

  @inline private val BYTES = N * 8 + 4

  @inline private def mag01(x: Long) = if ((x & 1) == 0) 0L else 0xb5026f5aa96619eL

  def randomSeed(): (Array[Long], Int) = (Utils.seedFromLong(N, Utils.longFromTime()), N + 1)

  def fromSeed(seed: (Array[Long], Int)): MersenneTwister64 =
    seed match {
      case (mt, mti) =>
        assert(mt.length == N)
        new MersenneTwister64(mt, mti)
    }

  def fromArray(arr: Array[Long]): MersenneTwister64 = fromSeed((Utils.seedFromArray(N, arr), N + 1))

  def fromBytes(bytes: Array[Byte]): MersenneTwister64 = fromArray(Pack.longsFromBytes(bytes, bytes.length / 8))

  def fromTime(time: Long = System.nanoTime): MersenneTwister64 = fromSeed(
    (Utils.seedFromLong(N, Utils.longFromTime(time)), N + 1)
  )
}
