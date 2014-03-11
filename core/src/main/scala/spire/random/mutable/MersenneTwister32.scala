/**
 * This is a 32-bit Scala implementation of MersenneTwister based on MT19937.c.
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
 * @see <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">Mersenne Twister Home Page</a>
 * @see <a href="http://en.wikipedia.org/wiki/Mersenne_twister">Mersenne Twister @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */

package spire.random.mutable

import spire.util.Pack
import scala.math.max
import java.nio.ByteBuffer
import java.util.Arrays

final class MersenneTwister32(private var seed: Int = 5489) extends IntBasedGenerator {
  @inline private def mag01(x: Int) = if((x & 1) == 0) 0 else 0x9908B0DF

  private val UpperMask = 0x80000000
  private val LowerMask = 0x7fffffff

  private val N = 624
  private val M = 397

  private val mt = new Array[Int](N)
  private var mti = N + 1

  this.seed(seed)

  def this(seed: Array[Int]) {
    this(19650218)
    this.seed(seed)
  }

  @inline private def seed(seed: Int) {
    mt(0) = seed

    for (i <- 1 until N) {
      mt(i) = 1812433253 * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i
    }
  }

  @inline private def seed(seed: Array[Int]) {
    var i = 1
    var j = 0
    var k = max(N, seed.length)

    while (k != 0) {
      mt(i) = (mt(i) ^ ((mt(i - 1) ^ (mt(i - 1) >>> 30)) * 1664525)) + seed(j) + j
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
      mt(i) = (mt(i) ^ ((mt(i - 1) ^ (mt(i - 1) >>> 30)) * 1566083941)) - i
      i += 1

      if (i >= N) {
        mt(0) = mt(N - 1)
        i = 1
      }

      k -= 1
    }

    mt(0) = 0x80000000 // MSB is 1; assuring non-zero initial array
  }

  def copyInit: MersenneTwister32 = {
    val copy = new MersenneTwister32(seed)
    for (i <- 0 until N) {
      copy.mt(i) = mt(i)
      copy.mti = mti
    }
    copy
  }

  private val BYTES = N * 4 + 4

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)
    for (i <- 0 until N) {
      bb.putInt(mt(i))
    }
    bb.putInt(mti)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]) {
    N * 8 + 4
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    for (i <- 0 until N) {
      mt(i) = bb.getInt()
    }
    mti = bb.getInt
  }

  // Generates the next random integer in the sequence
  def nextInt(): Int = {
    var y = 0

    if (mti >= N) {
      var kk = 0

      while (kk < N - M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y & 0x1)
        kk += 1
      }

      while (kk < N - 1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (y >>> 1) ^ mag01(y & 0x1)
        kk += 1
      }

      y = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (y >>> 1) ^ mag01(y & 0x1)

      mti = 0
    }

    y = mt(mti);
    mti += 1

    // Tempering
    y ^= (y >>> 11)
    y ^= (y <<   7) & 0x9D2C5680
    y ^= (y <<  15) & 0xEFC60000
    y ^= (y >>> 18)

    y
  }
}

object MersenneTwister32 extends GeneratorCompanion[MersenneTwister32, Int] {
  @volatile private var seedUniquifier = 8682522807148012L;

  def randomSeed(): Int =  { seedUniquifier += 1; (seedUniquifier + System.nanoTime).toInt }

  def fromBytes(bytes: Array[Byte]): MersenneTwister32 = new MersenneTwister32(Pack.intsFromBytes(bytes, bytes.length / 4))
  def fromSeed(seed: Int): MersenneTwister32 = new MersenneTwister32(seed)
  def fromTime(time: Long = System.nanoTime) = new MersenneTwister32({seedUniquifier += 1; (seedUniquifier + time).toInt})
}
