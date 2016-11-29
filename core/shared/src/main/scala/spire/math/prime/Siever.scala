package spire
package math.prime

import spire.math.SafeLong

import SieveUtil._

/**
 * Segmented Stream of Eratosthenes implementation
 *
 * This section really needs some good comments.
 *
 * Some future optimizations:
 *
 * 0. Consider an option to use multiple threads
 * 1. Faster heap/priority queue
 * 2. Tune chunkSize
 * 3. Use Long internally until we have to switch to SafeLong.
 * 4. Compress the amount of space our heaps take up.
 * 5. Read more efficient segmented sieves to get other ideas.
 * 6. Try using a delta-encoded prime log
 *
 * Obviously we are trying to be a bit more flexible than a
 * traditional prime finder that knows ahead of time what range it
 * will be operating over, which will hurt performance a bit. Also,
 * it's not written in C/assembly. So it will probably never be truly
 * competitive, but I'd like us to do as well as possible.
 */

/**
 * The Siever manages the segmented sieve process.
 *
 * At any given time, it holds onto a single sieve segment. Thus, the
 * siever should be used for a single lookup or traversal.
 *
 * Sievers are built using 'chunkSize' and 'cutoff' parameters. These
 * are passed along to any sieve segments they create. When possible,
 * it's probably better to use methods on the companion object, which
 * will instantiate a Siever for you with reasonable parameters.
 */
case class Siever(chunkSize: Int, cutoff: SafeLong) {
  require(chunkSize % 480 == 0, "chunkSize must be a multiple of 480")

  val arr = BitSet.alloc(chunkSize)
  var start: SafeLong = SafeLong(0)
  var limit: SafeLong = start + chunkSize
  val fastq: FastFactors = FastFactors.empty
  val slowq: FactorHeap = new FactorHeap
  var sieve: SieveSegment = SieveSegment(start, arr, cutoff)
  sieve.init(fastq, slowq)

  def largestBelow(n: SafeLong): SafeLong = {
    if (n < 3) throw new IllegalArgumentException("invalid argument: %s" format n)
    if (n == 3) return SafeLong(2)

    var i = 3
    var k = n - 1
    var last = SafeLong(2)
    while (true) {
      val primes = sieve.primes
      val len = primes.length
      if (n - start < len) {
        var i = 1
        val goal = (n - start).toInt
        while (i < goal) {
          if (primes(i)) last = start + i
          i += 2
        }
        return last
      } else {
        var i = len - 1
        while (1 <= i && !primes(i)) i -= 2
        if (1 <= i) last = start + i
      }
      initNextSieve()
      i = 1
    }
    return SafeLong(0) // impossible
  }

  def nth(n: Long): SafeLong = {
    if (n == 1) return SafeLong(2)
    var i = 3
    var k = n - 1
    while (true) {
      val primes = sieve.primes
      val len = primes.length
      while (i < len) {
        if (primes(i)) {
          k -= 1
          if (k < 1) return sieve.start + i
        }
        i += 2
      }
      initNextSieve()
      i = 1
    }
    return SafeLong(0) // impossible
  }

  private def initNextSieve(): Unit = {
    start += chunkSize
    limit += chunkSize
    val csq = cutoff ** 2
    if (limit >= csq) sys.error("too big: %s > %s (%s)" format (limit, csq, cutoff))
    arr.clear()
    sieve = SieveSegment(start, arr, cutoff)
    sieve.init(fastq, slowq)
  }

  def nextAfter(n: SafeLong): SafeLong = {
    var nn = sieve.nextAfter(n)
    while (nn == -1L) {
      initNextSieve()
      nn = sieve.nextAfter(start - 1)
    }
    nn
  }

  def streamAfter(p0: SafeLong): Stream[SafeLong] = {
    val p = nextAfter(p0)
    p #:: streamAfter(p)
  }

  def arrayAt(p: SafeLong, size: Int): Array[SafeLong] = {
    val arr = new Array[SafeLong](size)
    def loop(i: Int, p: SafeLong): Unit =
      if (i < arr.length) {
        arr(i) = p
        loop(i + 1, nextAfter(p))
      }
    loop(0, p)
    arr
  }
}
