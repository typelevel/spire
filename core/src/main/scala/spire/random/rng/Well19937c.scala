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
import java.util

/**
 * This is a Scala implementation of the Well19937c PRNG based on WELL19937a.c.
 *
 * <p>The acronym WELL stands for Well Equidistributed Long-period Linear.
 *
 * <p><b>Reference: </b> François Panneton, Pierre L'Ecuyer and Makoto Matsumoto: "Improved Long-Period Generators Based
 * on Linear Recurrences Modulo 2", <i>ACM Transactions on Mathematical Software,</i> Vol. 32, No. 1, January 2006, pp
 * 1--16.
 *
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/well/WELL19937a.c">WELL19937a.c</a>
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/WELLRNG.html">Well PRNG Home Page</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Well_Equidistributed_Long-period_Linear">WELL @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Dušan Kysel</a>
 */
final class Well19937c protected[random] (state: Array[Int], i0: Int) extends IntBasedGenerator {

  import Well19937c.{mat0neg, mat0pos, mat1, mat3pos, BYTES, LowerMask, R, TemperB, TemperC, UpperMask}

  private var i: Int = i0

  def copyInit: Well19937c = new Well19937c(state.clone, i)

  def getSeedBytes: Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    cfor(0)(_ < R, _ + 1) { i => bb.putInt(state(i)) }
    bb.putInt(i)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs = if (bytes.length < BYTES) util.Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)

    cfor(0)(_ < R, _ + 1) { i => state(i) = bb.getInt }
    i = bb.getInt
  }

  def nextInt(): Int = {

    import Well19937acIndexCache._

    val z0: Int = (state(vrm1(i)) & LowerMask) | (state(vrm2(i)) & UpperMask)
    val z1: Int = mat0neg(-25, state(i)) ^ mat0pos(27, state(vm1(i)))
    val z2: Int = mat3pos(9, state(vm2(i))) ^ mat0pos(1, state(vm3(i)))

    state(i) = z1 ^ z2
    state(vrm1(i)) = mat1(z0) ^ mat0neg(-9, z1) ^ mat0neg(-21, z2) ^ mat0pos(21, state(i))
    i = vrm1(i)

    // Matsumoto-Kurita tempering to get a ME (maximally equidistributed) generator
    val t0 = state(i)
    val t1 = t0 ^ ((t0 << 7) & TemperB)
    val t2 = t1 ^ ((t1 << 15) & TemperC)

    t2
  }
}

object Well19937c extends GeneratorCompanion[Well19937c, (Array[Int], Int)] {

  @inline private val UpperMask = 0x7fffffff // = 0xFFFFFFFF ^ Int.MinValue
  @inline private val LowerMask = 0x80000000 // = Int.MinValue

  @inline private val TemperB = 0xe46e1700
  @inline private val TemperC = 0x9b868000

  // Number of bits in the pool.
  @inline final private val K: Int = 19937

  // Length of the pool in ints.
  @inline final private val R: Int = (K + 31) / 32

  // Length of the pool in ints -1.
  // @inline private final val R_1 : Int = R - 1

  // Length of the pool in ints -2.
  // @inline private final val R_2 : Int = R - 2

  // Length of the pool and index in bytes
  @inline final private val BYTES = R * 4 + 4

  // First parameter of the algorithm.
  // @inline private final val M1 : Int = 70

  // Second parameter of the algorithm.
  // @inline private final val M2 : Int = 179

  // Third parameter of the algorithm.
  // @inline private final val M3 : Int = 449

  @inline final private def mat0pos(t: Int, v: Int) = v ^ (v >>> t)
  @inline final private def mat0neg(t: Int, v: Int) = v ^ (v << -t)
  @inline final private def mat1(v: Int) = v
  @inline final private def mat3pos(t: Int, v: Int) = v >>> t

  def randomSeed(): (Array[Int], Int) = (Utils.seedFromInt(R, Utils.intFromTime()), 0)

  def fromSeed(seed: (Array[Int], Int)): Well19937c =
    seed match {
      case (state, stateIndex) =>
        assert(state.length == R)
        new Well19937c(state, stateIndex)
    }

  def fromArray(arr: Array[Int]): Well19937c =
    fromSeed((Utils.seedFromArray(R, arr), 0))

  def fromBytes(bytes: Array[Byte]): Well19937c =
    fromArray(Pack.intsFromBytes(bytes, bytes.length / 4))

  def fromTime(time: Long = System.nanoTime): Well19937c =
    fromSeed((Utils.seedFromInt(R, Utils.intFromTime(time)), 0))
}
