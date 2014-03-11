/**
 * This is a 64-bit Scala implementation of MersenneTwister based on MT19937-64.c.
 *
 * <p>MersenneTwister is a fast, 623-dimensionally equidistributed pseudo random number generator
 * with a <tt>2<sup>19937</sup>&nbsp;-&nbsp;1</tt> long period.
 *
 * <p><b>Reference: </b>
 * Makato Matsumoto and Takuji Nishimura:
 * "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform Pseudo-Random Number Generator",
 * <i>ACM Transactions on Modeling and Computer Simulation,</i> Vol. 8, No. 1, January 1998, pp 3--30.
 *
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/mt19937-64.c">MT19937-64.c</a>
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">Mersenne Twister Home Page</a>
 * @see <a href="http://en.wikipedia.org/wiki/Mersenne_twister">Mersenne Twister @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */

package spire.random
package rng

import spire.util.Pack
import scala.math.max
import java.nio.ByteBuffer
import java.util.Arrays


final class MersenneTwister64(private var seed: Long = 5489) extends LongBasedGenerator {
  @inline private def mag01(x: Long) = if((x & 1) == 0) 0L else 0xB5026F5AA96619EL

  private val UpperMask = 0xFFFFFFFF80000000L
  private val LowerMask = 0x7FFFFFFFL

  private val N = 312
  private val M = 156

  private val mt = new Array[Long](N)
  private var mti = N + 1

  this.seed(seed)

  def this(seed: Array[Long]) {
    this(19650218)
    this.seed(seed)
  }

  @inline private def seed(seed: Long) {
    mt(0) = seed

    for (i <- 1 until N) {
      mt(i) = 6364136223846793005L * (mt(i - 1) ^ (mt(i - 1) >>> 62)) + i
    }
  }

  @inline private def seed(seed: Array[Long]) {
    var i = 1
    var j = 0
    var k = max(N, seed.length)

    while (k != 0) {
      mt(i) = (mt(i) ^ ((mt(i - 1) ^ (mt(i - 1) >>> 62)) * 3935559000370003845L)) + seed(j) + j
      i += 1
      j += 1

      if (i >= N) {
        mt(0) = mt(N - 1)
        i = 1
      }

      if (j >= seed.length) {
        j = 0
      }
      k -= 1
    }

    k = N - 1
    while (k != 0) {
      mt(i) = (mt(i) ^ ((mt(i - 1) ^ (mt(i - 1) >>> 62)) * 2862933555777941757L)) - i
      i += 1

      if (i >= N) {
        mt(0) = mt(N - 1)
        i = 1
      }

      k -= 1
    }

    mt(0) = 1L << 63 // MSB is 1; assuring non-zero initial array
  }

  def copyInit: MersenneTwister64 = {
    val copy = new MersenneTwister64(seed)
    for (i <- 0 until N) {
      copy.mt(i) = mt(i)
      copy.mti = mti
    }
    copy
  }

  private val BYTES = N * 8 + 4

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)
    for (i <- 0 until N) {
      bb.putLong(mt(i))
    }
    bb.putInt(mti)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]) {
    N * 8 + 4
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    for (i <- 0 until N) {
      mt(i) = bb.getLong()
    }
    mti = bb.getInt
  }

  // Generates the next random long in the sequence
  override def nextLong(): Long = {
    var x = 0L

    if (mti >= N) {
      if (mti == N + 1) {
        seed(5489)
      }

      var kk = 0

      while (kk < N - M) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (x >>> 1) ^ mag01(x & 0x1)
        kk += 1
      }

      while (kk < N - 1) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (x >>> 1) ^ mag01(x & 0x1)
        kk += 1
      }

      x = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (x >>> 1) ^ mag01(x & 0x1)

      mti = 0
    }

    x = mt(mti);
    mti += 1

    // Tempering
    x ^= (x >>> 29) & 0x5555555555555555L
    x ^= (x  << 17) & 0x71D67FFFEDA60000L
    x ^= (x  << 37) & 0xFFF7EEE000000000L
    x ^= (x >>> 43)

    x
  }
}

object MersenneTwister64 extends GeneratorCompanion[MersenneTwister64, Long] {
  @volatile private var seedUniquifier = 8682522807148012L;

  def randomSeed(): Long =  { seedUniquifier += 1; (seedUniquifier + System.nanoTime) }

  def fromBytes(bytes: Array[Byte]): MersenneTwister64 = new MersenneTwister64(Pack.longsFromBytes(bytes, bytes.length / 8))
  def fromSeed(seed: Long): MersenneTwister64 = new MersenneTwister64(seed)
  def fromTime(time: Long = System.nanoTime) = new MersenneTwister64({seedUniquifier += 1; seedUniquifier + time})
}
