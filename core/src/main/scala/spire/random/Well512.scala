package spire.random

import scala.annotation.tailrec

import java.nio.ByteBuffer
import java.util.Arrays

/**
 * Implements the WELL PRNG (Well Equidistributed Long-period Linear),
 * developed by F. Panneton, P. L'Ecuyer, and M. Matsumoto.
 * 
 * This class uses WELL512a, which contains 512 bits of state.
 */
class Well512 protected[random] (_i: Int, private var state: Array[Int]) extends IntBasedGenerator {
  var i: Int = _i

  @inline final def m1 = 13
  @inline final def m2 = 9

  @inline final def mat0pos(t: Int, v: Int) = v ^ (v >>> t)
  @inline final def mat0neg(t: Int, v: Int) = v ^ (v << -t)
  @inline final def mat3neg(t: Int, v: Int) = v << -t
  @inline final def mat4neg(t: Int, b: Int, v: Int) = v ^ ((v << -t) & b)

  @inline final def v0(i: Int) = state(i)
  @inline final def vm1(i: Int) = state((i + m1) & 0x0f)
  @inline final def vm2(i: Int) = state((i + m2) & 0x0f)
  @inline final def vrm1(i: Int) = state((i + 15) & 0x0f)

  /**
   * Generates a copy of this PRNG (with independent state).
   */
  def copy: Well512 = new Well512(i, state.clone)

  /**
   * Generate a copy of the RNG's internal state.
   */
  def copyState: Array[Int] = state.clone

  def getSeed(): Array[Int] = state.clone

  def setSeed(ints: Array[Int]): Unit = state = ints

  def getSeedBytes(): Array[Byte] =
    Pack.intsToBytes(state)

  def setSeedBytes(bytes: Array[Byte]): Unit =
    state = Pack.intsFromBytes(bytes, 16)

  /**
   * Generates a random int. All 32-bit int values are equally likely.
   */
  def nextInt(): Int = {
    val z0: Int = vrm1(i)
    val z1: Int = mat0neg(-16, v0(i)) ^ mat0neg(-15, vm1(i))
    val z2: Int = mat0pos(11, vm2(i))
    state(i) = z1 ^ z2
    state((i + 15) & 0x0f) = (
      mat0neg(-2, z0) ^
      mat0neg(-18, z1) ^
      mat3neg(-28, z2) ^
      mat4neg(-5, 0xda442d24, state(i))
    )
    i = (i + 15) & 0x0f
    state(i)
  }
}

object Well512 extends GeneratorCompanion[Well512, Array[Int]] {

  def randomSeed(): Array[Int] = global.generateInts(16)

  def fromBytes(bytes: Array[Byte]): Well512 = {
    val bs = if (bytes.length < 64) Arrays.copyOf(bytes, 64) else bytes
    new Well512(0, Pack.intsFromBytes(bytes, 16))
  }

  def fromSeed(seed: Array[Int]) = new Well512(0, seed)

  def fromTime(time: Long = System.nanoTime()): Well512 =
    new Well512(0, Lcg64.fromTime(time).generateInts(16))
}
