package spire.math.prime

import spire.math.{SafeLong, sqrt, max, log}

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
package object sieve {

  private[prime] val SieveSize = 9600 * 1000

  def sieverForNth(n: Long): Siever = {
    val upper = n * log(n) + n * log(log(n - 0.9385))
    val cutoff = max(1000L, (sqrt(upper) + 512L).toLong)
    Siever(SieveSize, cutoff)
  }

  def nth(n: Long): SafeLong =
    sieverForNth(n).nth(n)

  import SafeLong.{two, three}

  def fill(n: Int): Array[SafeLong] = {
    if (n <= 0) throw new IllegalArgumentException(n.toString)
    else if (n == 1) Array(two)
    else {
      val siever = sieverForNth(n)
      val arr = new Array[SafeLong](n)
      arr(0) = two
      arr(1) = three
      def loop(i: Int, last: SafeLong): Unit =
        if (i < arr.length) {
          val p = siever.nextAfter(last)
          arr(i) = p
          loop(i + 1, p)
        }
      loop(2, three)
      arr
    }
  }

  def stream: Stream[SafeLong] =
    stream(SieveSize, SafeLong(1000000))

  def stream(chunkSize: Int, cutoff: SafeLong): Stream[SafeLong] =
    two #:: three #:: Siever(chunkSize, cutoff).streamAfter(three)
}
