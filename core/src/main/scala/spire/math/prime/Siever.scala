package spire.math.prime

import spire.math.{SafeLong, log, max}

import SieveUtil._

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
  if (chunkSize % 480 != 0) sys.error("chunkSize must be a multiple of 480")

  val arr = BitSet.alloc(chunkSize)
  var start: SafeLong = SafeLong(0)
  var limit: SafeLong = start + chunkSize
  val fastq: FastFactors = FastFactors.empty
  val slowq: FactorHeap = new FactorHeap
  var sieve: SieveSegment = SieveSegment(start, arr, cutoff)
  sieve.init(fastq, slowq)

  def largestBelow(n: SafeLong): SafeLong = {
    if (n < 3) sys.error("invalid argument: %s" format n)
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

  private def initNextSieve() {
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
}
