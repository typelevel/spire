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
package mutable

import spire.syntax.cfor._
import spire.util.Pack
import java.nio.ByteBuffer
import java.util

/**
 * This is a Scala implementation of the Well512a PRNG based on WELL512a.c.
 *
 * <p>The acronym WELL stands for Well Equidistributed Long-period Linear.
 *
 * <p><b>Reference: </b>
 * Fran&ccedil;ois Panneton, Pierre L'Ecuyer and Makoto Matsumoto:
 * "Improved Long-Period Generators Based on Linear Recurrences Modulo 2",
 * <i>ACM Transactions on Mathematical Software,</i> Vol. 32, No. 1, January 2006, pp 1--16.
 *
 * @see <a href="http://www.iro.umontreal.ca/~panneton/well/WELL512a.c">WELL512a.c</a>
 * @see <a href="http://www.iro.umontreal.ca/~panneton/WELLRNG.html">Well PRNG Home Page</a>
 * @see <a href="http://en.wikipedia.org/wiki/Well_Equidistributed_Long-period_Linear">WELL @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
final class Well512a protected[random](state: Array[Int], i0: Int) extends IntBasedGenerator {

  import Well512a.{R, R_1, BYTES, M1, M2, mat0pos, mat0neg, mat3neg, mat4neg}

  /*
    @inline private final val v0    = new Utils.IntArrayWrapper(i => i, state)
    @inline private final val vm1   = new Utils.IntArrayWrapper(i => (i + M1)  & R_1, state)
    @inline private final val vm2   = new Utils.IntArrayWrapper(i => (i + M2)  & R_1, state)
    @inline private final val vrm1  = new Utils.IntArrayWrapper(i => (i + R_1) & R_1, state)
    @inline private final val newV0 = vrm1
    @inline private final val newV1 = v0
  */

  private var i : Int = i0

  def copyInit: Well512a = new Well512a(state.clone(), i)

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    cfor(0)(_ < R, _ + 1) { i => bb.putInt(state(i)) }
    bb.putInt(i)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]) {
    val bs = if (bytes.length < BYTES) util.Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)

    cfor(0)(_ < R, _ + 1) { i => state(i) = bb.getInt }
    i = bb.getInt
  }

  /**
   * Generate an equally-distributed random Int.
   */
  def nextInt(): Int = {

    @inline def map(r: Int) = (i + r) & R_1

    val z0: Int = state(map(R_1))
    val z1: Int = mat0neg(-16, state(i)) ^ mat0neg(-15, state(map(M1)))
    val z2: Int = mat0pos(11, state(map(M2)))

    state(i) = z1 ^ z2
    state(map(R_1)) = mat0neg(-2, z0) ^ mat0neg(-18, z1) ^ mat3neg(-28, z2) ^ mat4neg(-5, 0xda442d24, state(i))
    i = map(R_1)

    /*
      val z0: Int = vrm1(i)
      val z1: Int = mat0neg(-16, v0(i)) ^ mat0neg(-15, vm1(i))
      val z2: Int = mat0pos(11, vm2(i))

      newV1(i) = z1 ^ z2
      newV0(i) = mat0neg(-2, z0) ^ mat0neg(-18, z1) ^ mat3neg(-28, z2) ^ mat4neg(-5, 0xda442d24, newV1(i))
      i = (i + R_1) & R_1
    */

    state(i)
  }
}

object Well512a extends GeneratorCompanion[Well512a, (Array[Int], Int)] {

  /** Number of bits in the pool. */
  @inline private final val K : Int = 512

  /** Length of the pool in ints. */
  @inline private final val R : Int = K / 32

  /** Length of the pool in ints -1. */
  @inline private final val R_1 : Int = R - 1

  /** Length of the pool and index in bytes */
  @inline private final val BYTES = R * 4 + 4

  /** First parameter of the algorithm. */
  @inline private final val M1 : Int = 13

  /** Second parameter of the algorithm. */
  @inline private final val M2 : Int = 9

  /** Third parameter of the algorithm. */
  // @inline private final val M3 : Int = 5

  @inline private final def mat0pos(t: Int, v: Int)         = v ^ (v >>> t)
  @inline private final def mat0neg(t: Int, v: Int)         = v ^ (v << -t)
  @inline private final def mat3neg(t: Int, v: Int)         = v << -t
  @inline private final def mat4neg(t: Int, b: Int, v: Int) = v ^ ((v << -t) & b)
                                                                                   
  def randomSeed(): (Array[Int], Int) = (Utils.seedFromInt(R, Utils.intFromTime()), 0)

  def fromSeed(seed: (Array[Int], Int)): Well512a =
    seed match {
      case (state, stateIndex) =>
        assert(state.length == R)
        new Well512a(state, stateIndex)
    }

  def fromArray(arr: Array[Int]): Well512a = fromSeed(Utils.seedFromArray(R, arr), 0)

  def fromBytes(bytes: Array[Byte]): Well512a = fromArray(Pack.intsFromBytes(bytes, bytes.length / 4))

  def fromTime(time: Long = System.nanoTime) : Well512a = fromSeed(Utils.seedFromInt(R, Utils.intFromTime(time)), 0)
}
