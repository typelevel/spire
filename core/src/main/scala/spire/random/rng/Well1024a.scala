/**
 * **********************************************************************\ * Project ** * ______ ______ __ ______ ____
 * ** * / ____/ / __ / / / / __ / / __/ (c) 2011-2014 ** * / /__ / /_/ / / / / /_/ / / /_ ** * /___ / / ____/ / / / __ /
 * / __/ Erik Osheim, Tom Switzer ** * ____/ / / / / / / / | | / /__ ** * /_____/ /_/ /_/ /_/ |_| /____/ All rights
 * reserved. ** * ** * Redistribution and use permitted under the MIT license. ** * **
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
 * This is a Scala implementation of the Well1024a PRNG based on WELL1024a.c.
 *
 * <p>The acronym WELL stands for Well Equidistributed Long-period Linear.
 *
 * <p><b>Reference: </b> François Panneton, Pierre L'Ecuyer and Makoto Matsumoto: "Improved Long-Period Generators Based
 * on Linear Recurrences Modulo 2", <i>ACM Transactions on Mathematical Software,</i> Vol. 32, No. 1, January 2006, pp
 * 1--16.
 *
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/well/WELL1024a.c">WELL1024a.c</a>
 * @see
 *   <a href="http://www.iro.umontreal.ca/~panneton/WELLRNG.html">Well PRNG Home Page</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Well_Equidistributed_Long-period_Linear">WELL @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Dušan Kysel</a>
 */
final class Well1024a protected[random] (state: Array[Int], i0: Int) extends IntBasedGenerator {

  import Well1024a.{mat0neg, mat0pos, BYTES, M1, M2, M3, R, R_1}

  // @inline private final val v0   = new Utils.IntArrayWrapper(i => i, state)
  // @inline private final val vm1  = new Utils.IntArrayWrapper(i => (i + M1)  & R_1, state)
  // @inline private final val vm2  = new Utils.IntArrayWrapper(i => (i + M2)  & R_1, state)
  // @inline private final val vm3  = new Utils.IntArrayWrapper(i => (i + M3)  & R_1, state)
  // @inline private final val vrm1 = new Utils.IntArrayWrapper(i => (i + R_1) & R_1, state)
  // @inline private final val newV0 = vrm1
  // @inline private final val newV1 = v0

  private var i: Int = i0

  def copyInit: Well1024a = new Well1024a(state.clone, i)

  def getSeedBytes: Array[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](BYTES)
    val bb: ByteBuffer = ByteBuffer.wrap(bytes)

    cfor(0)(_ < R, _ + 1) { i => bb.putInt(state(i)) }
    bb.putInt(i)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): Unit = {
    val bs: Array[Byte] = if (bytes.length < BYTES) util.Arrays.copyOf(bytes, BYTES) else bytes
    val bb: ByteBuffer = ByteBuffer.wrap(bs)

    cfor(0)(_ < R, _ + 1) { i => state(i) = bb.getInt }
    i = bb.getInt
  }

  def nextInt(): Int = {

    @inline def map(r: Int): Int = (i + r) & R_1

    val z0: Int = state(map(R_1))
    val z1: Int = state(i) ^ mat0pos(8, state(map(M1)))
    val z2: Int = mat0neg(-19, state(map(M2))) ^ mat0neg(-14, state(map(M3)))

    state(i) = z1 ^ z2
    state(map(R_1)) = mat0neg(-11, z0) ^ mat0neg(-7, z1) ^ mat0neg(-13, z2)
    i = map(R_1)

    // val z0: Int = vrm1(i)
    // val z1: Int = v0(i) ^ mat0pos(8, vm1(i))
    // val z2: Int = mat0neg(-19, vm2(i)) ^ mat0neg(-14, vm3(i))
    //
    // newV1(i) = z1 ^ z2
    // newV0(i) = mat0neg(-11, z0) ^ mat0neg(-7, z1) ^ mat0neg(-13, z2)
    // i = (i + R_1) & R_1

    state(i)
  }
}

object Well1024a extends GeneratorCompanion[Well1024a, (Array[Int], Int)] {

  // Number of bits in the pool.
  @inline final private val K: Int = 1024

  // Length of the pool in ints.
  @inline final private val R: Int = K / 32

  // Length of the pool in ints -1.
  @inline final private val R_1: Int = R - 1

  // Length of the pool and index in bytes.
  @inline final private val BYTES: Int = R * 4 + 4

  // First parameter of the algorithm.
  @inline final private val M1: Int = 3

  // Second parameter of the algorithm.
  @inline final private val M2: Int = 24

  // Third parameter of the algorithm.
  @inline final private val M3: Int = 10

  @inline final private def mat0pos(t: Int, v: Int): Int = v ^ (v >>> t)
  @inline final private def mat0neg(t: Int, v: Int): Int = v ^ (v << -t)

  def randomSeed(): (Array[Int], Int) =
    (Utils.seedFromInt(R, Utils.intFromTime()), 0)

  def fromSeed(seed: (Array[Int], Int)): Well1024a =
    seed match {
      case (state, stateIndex) =>
        assert(state.length == R)
        new Well1024a(state, stateIndex)
    }

  def fromArray(arr: Array[Int]): Well1024a =
    fromSeed((Utils.seedFromArray(R, arr), 0))

  def fromBytes(bytes: Array[Byte]): Well1024a =
    fromArray(Pack.intsFromBytes(bytes, bytes.length / 4))

  def fromTime(time: Long = System.nanoTime): Well1024a =
    fromSeed((Utils.seedFromInt(R, Utils.intFromTime(time)), 0))
}
