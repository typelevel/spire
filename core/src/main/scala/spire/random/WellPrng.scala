package spire.random

import scala.annotation.tailrec

/**
 * Implements the WELL PRNG (Well Equidistributed Long-period Linear),
 * developed by F. Panneton, P. L'Ecuyer, and M. Matsumoto.
 * 
 * This class uses WELL512a, which contains 512 bits of state.
 */
class WellPrng protected[random] (_i: Int, state: Array[Int]) extends IntGenerator {
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
  def copy: WellPrng = new WellPrng(i, state.clone)

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

object WellPrng {
  @inline final def size = 16

  /**
   * Produce a random seed array for use constructing WellPrng instances.
   */
  def randomSeed(): Array[Int] = {
    import scala.util.Random.nextInt
    val data = new Array[Int](size)
    for (i <- 0 until size) data(i) = nextInt()
    data
  }

  def apply(): WellPrng = new WellPrng(0, randomSeed)
}

/**
 * Global PRNG object.
 *
 * Due to possible use by several threads, this object synchronizes on all PRNG
 * access. To reduce possible lock contention, individual threads may
 * instantiate their own WellPrng instances, which are no synchronized.
 */
object global extends WellPrng(0, WellPrng.randomSeed) {
  override def nextInt(): Int = this.synchronized(super.nextInt())
}
