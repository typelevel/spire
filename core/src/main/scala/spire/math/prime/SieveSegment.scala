package spire.math.prime

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer}
import System.arraycopy

import spire.math.{SafeLong, min}
import spire.syntax.cfor._

import SieveUtil._

/**
 * This respresents a single sieve segment.
 *
 * The 'start' field says what this segment's first number
 * is. 'primes' is a bitset of possible primes in this
 * segment. 'cutoff' specifies the largest prime factor we're
 * interested in. This means that cutoff**2-1 is the largest number we
 * could reliably identify as prime.
 * 
 * We are using a mod30 wheel, which means that we don't need to
 * manually factor using 2, 3, or 5 (30 is the lcm of 2, 3, and
 * 5). Since each wheel turn is 30-bits, and our bitset groups
 * elements into 32-bit groups (integers), we have a 480-bit (15
 * integer) period between the wheel and the bitset. This requires our
 * segment length to be divisible by 480.
 * 
 * When building a sieve, we will first initialize using the mod30
 * wheel. Then, if we are on the first segment, we'll do a traditional
 * sieve. We'll save any primes greater than 5 we find as factors,
 * either fast factors (if they will show up frequently in each
 * segment) or slow factors otherwise. If a factor is larger than
 * cutoff we don't save it. After that we'll be done with the first
 * segment.
 *
 * For later segments, we will use our fast and slow factors to block
 * out composites as we find them. Like in the first segment, we'll
 * save factors we find (although any new factors we find now will
 * always be slow). And of course we won't save any factors above our
 * cutoff.
 *
 * Once the sieve is initialized it doesn't do anything else
 * interesting, besides report prime numbers. Currently its internals
 * are made available to the Siever.
 */
object SieveSegment {
  val wheel30: Array[Int] = {
    var b: Long = 0L
    b |= (1 << 1)
    b |= (1 << 7)
    b |= (1 << 11)
    b |= (1 << 13)
    b |= (1 << 17)
    b |= (1 << 19)
    b |= (1 << 23)
    b |= (1 << 29)
    val n: Long = b | (b << 30L)
    val arr = new Array[Int](15)
    cfor(0)(_ < 15, _ + 1) { i =>
      arr(i) = ((n >>> (i * 2)) & 0xffffffffL).toInt
    }
    arr
  }

}

case class SieveSegment(start: SafeLong, primes: BitSet, cutoff: SafeLong) {
  def isPrime(n: SafeLong): Boolean = primes((n - start).toInt)
  def isComposite(n: SafeLong): Boolean = !primes((n - start).toInt)
  def set(n: SafeLong): Unit = primes += (n - start).toInt
  def unset(n: SafeLong): Unit = primes -= (n - start).toInt

  def nextAfter(n: SafeLong): SafeLong = {
    var i = (n - start + 2).toInt
    val len = primes.length
    while (i < len) {
      if (primes(i)) return start + i
      i += 2
    }
    SafeLong(-1L) // fail
  }

  def init(fastq: FastFactors, slowq: FactorHeap) {
    initMod30()
    if (start == 0) {
      initFirst(fastq, slowq)
    } else {
      val limit = min(cutoff ** 2, start + primes.length)
      initFromArray(fastq)
      initFromQueue(limit, slowq)
      initRest(slowq)
    }
  }

  def initMod30() {
    val arr = primes.array
    assert(arr.length % 15 == 0)
    val limit = arr.length
    val wheel = SieveSegment.wheel30
    cfor(0)(_ < limit, _ + 15)(i => arraycopy(wheel, 0, arr, i, 15))
    if (start == 0L) {
      primes -= 1
      primes += 2
      primes += 3
      primes += 5
    }
  }

  private def initFromArray(fastq: FastFactors) {
    val arr = fastq.arr
    var i = 0

    val len: Long = if (start + primes.length < cutoff)
      (cutoff - start).toLong
    else
      primes.length

    while (i < arr.length) {
      val factor = arr(i)
      var j = (factor.m - start).toInt
      val k = factor.p
      val kk = k + k
      val lim = len - kk
      primes -= j
      while (j < lim) {
        j += kk
        primes -= j
      }
      factor.m = start + j + kk
      i += 1
    }
  }

  @tailrec private def initFromQueue(limit: SafeLong, q: FactorHeap) {
    if (q.isEmpty) return ()

    val factor = q.dequeue
    val m = factor.next
    if (m < limit) {
      val p = factor.p
      val len = primes.length
      var i = (m - start).toInt
      val m2 = if (p < len) {
        val k = p.toInt
        val kk = k + k
        while (i < len) { primes -= i; i += kk }
        start + i
      } else {
        primes -= i
        m + p
      }
      factor.next = m2
      q += factor
      initFromQueue(limit, q)
    } else {
      q += factor
    }
  }

  def initFirst(fastq: FastFactors, slowq: FactorHeap) {
    var p: Int = 1
    val len = primes.length
    val buf = ArrayBuffer.empty[FastFactor]
    while (p < len) {
      if (primes(p)) {
        var m = p.toLong * p.toLong
        if (m < len) {
          val pp = p + p
          var k = m.toInt
          primes -= k
          val lim = len - pp
          while (k < lim) { k += pp; primes -= k }
          m = k.toLong + pp
        }
        if (p < 7) {
        } else if (m - primes.length < primes.length) {
          buf += FastFactor(p, SafeLong(m))
        } else if (cutoff > p) {
          slowq += Factor(SafeLong(p), SafeLong(m))
        }
      }
      p += 2
    }
    fastq.arr = buf.toArray
  }

  def initRest(slowq: FactorHeap) {
    if (start >= cutoff) return ()

    val len: Long = if (start + primes.length >= cutoff)
      (cutoff - start).toLong
    else
      primes.length

    var i = 1
    while (i < len) {
      if (primes(i)) {
        val p: SafeLong = start + i
        slowq += Factor(p, p ** 2)
      }
      i += 2
    }
  }
}
