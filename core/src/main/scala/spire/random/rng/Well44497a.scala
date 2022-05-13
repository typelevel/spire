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
 * This is a Scala implementation of the Well44497a PRNG based on WELL44497a.c.
 *
 * <p>The acronym WELL stands for Well Equidistributed Long-period Linear.
 *
 * <p><b>Reference: </b> François Panneton, Pierre L'Ecuyer and Makoto Matsumoto: "Improved Long-Period Generators Based
 * on Linear Recurrences Modulo 2", <i>ACM Transactions on Mathematical Software,</i> Vol. 32, No. 1, January 2006, pp
 * 1--16.
 *
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/well/WELL44497a.c">WELL44497a.c</a>
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/WELLRNG.html">Well PRNG Home Page</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Well_Equidistributed_Long-period_Linear">WELL @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Dušan Kysel</a>
 */
final class Well44497a protected[random] (state: Array[Int], i0: Int) extends IntBasedGenerator {

  import Well44497a.{mat0neg, mat0pos, mat1, mat3neg, mat5, BYTES, LowerMask, R, UpperMask}

  private var i: Int = i0

  def copyInit: Well44497a = new Well44497a(state.clone, i)

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

    import Well44497abIndexCache._

    val z0: Int = state(vrm1(i)) & LowerMask | state(vrm2(i)) & UpperMask
    val z1: Int = mat0neg(-24, state(i)) ^ mat0pos(30, state(vm1(i)))
    val z2: Int = mat0neg(-10, state(vm2(i))) ^ mat3neg(-26, state(vm3(i)))

    state(i) = z1 ^ z2
    state(vrm1(i)) = mat1(z0) ^ mat0pos(20, z1) ^ mat5(9, 0xb729fcec, 0xfbffffff, 0x00020000, z2) ^ mat1(state(i))
    i = vrm1(i)

    state(i)
  }
}

object Well44497a extends GeneratorCompanion[Well44497a, (Array[Int], Int)] {

  @inline private val UpperMask = 0xffffffff >>> 17
  @inline private val LowerMask = ~UpperMask

  // Number of bits in the pool.
  @inline final private val K: Int = 44497

  // Length of the pool in ints.
  @inline final private val R: Int = (K + 31) / 32

  // Length of the pool in ints -1.
  // @inline private final val R_1 : Int = R - 1

  // Length of the pool in ints -2.
  // @inline private final val R_2 : Int = R - 2

  // Length of the pool and index in bytes
  @inline final private val BYTES = R * 4 + 4

  // First parameter of the algorithm.
  // @inline private final val M1 : Int = 23

  // Second parameter of the algorithm.
  // @inline private final val M2 : Int = 481

  // Third parameter of the algorithm.
  // @inline private final val M3 : Int = 229

  @inline final private def mat0pos(t: Int, v: Int) = v ^ v >>> t
  @inline final private def mat0neg(t: Int, v: Int) = v ^ v << -t
  @inline final private def mat1(v: Int) = v
  // @inline private final def mat2(a: Int, v: Int)            = if ((v & 1) != 0) (v >>> 1) ^ a else v >>> 1
  // @inline private final def mat3pos(t: Int, v: Int)         = v >>> t
  @inline final private def mat3neg(t: Int, v: Int) = v << -t
  // @inline private final def mat4pos(t: Int, b: Int, v: Int) = v ^ ((v >>> t) & b)
  // @inline private final def mat4neg(t: Int, b: Int, v: Int) = v ^ ((v << -t) & b)
  @inline final private def mat5(r: Int, a: Int, ds: Int, dt: Int, v: Int) = {
    if ((v & dt) != 0) {
      (v << r ^ v >>> 32 - r) & ds ^ a
    } else {
      (v << r ^ v >>> 32 - r) & ds
    }
  }

  def randomSeed(): (Array[Int], Int) =
    (Utils.seedFromInt(R, Utils.intFromTime()), 0)

  def fromSeed(seed: (Array[Int], Int)): Well44497a =
    seed match {
      case (state, stateIndex) =>
        assert(state.length == R)
        new Well44497a(state, stateIndex)
    }

  def fromArray(arr: Array[Int]): Well44497a =
    fromSeed((Utils.seedFromArray(R, arr), 0))

  def fromBytes(bytes: Array[Byte]): Well44497a =
    fromArray(Pack.intsFromBytes(bytes, bytes.length / 4))

  def fromTime(time: Long = System.nanoTime): Well44497a =
    fromSeed((Utils.seedFromInt(R, Utils.intFromTime(time)), 0))
}
