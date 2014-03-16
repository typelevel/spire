package spire.math.prime

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, PriorityQueue}
import System.arraycopy

import spire.math._
import spire.syntax.cfor._
import spire.syntax.order._

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
 * 6. Try using adding delta-encoded prime log
 * 
 * Obviously InfStream has to be a bit more flexible than a
 * traditional prime finder that knows ahead of time what range it
 * will be operating over. Also, it's not written in C/assembly.
 * So it will probably never be truly competitive, but I'd 
 * like to do as well as possible.
 */

/**
 * For now we'll be using SafeLong as our boxed number.
 * 
 * It's essentially a boxed Long that will overflow to BigInt if needed.
 * This protects us when doing things like n ** 2.
 */
object Types { type N = SafeLong }

object N {
  def apply(n: Int) = SafeLong(n)
  def apply(n: Long) = SafeLong(n)
}

import Types._

/**
 * Reprsents a prime factor which we need to keep track of.
 * 
 * Similar to Factor, but in this case the prime is small enough that
 * it fits in an Int. This means that each of our sieve segments will
 * contain at least one multiple of 'p' if not more. So we can use a
 * slightly more compact data structure.
 */
case class FastFactor(p: Int, var m: N)

/**
 * This class simply wraps an Array[FastFactor]. Its only real purpose
 * is to allow us to lazily initialize our fast factors (which we can
 * only do after constructing our first sieve segment).
 */
object FastFactors {
  def empty = FastFactors(new Array[FastFactor](0))
}

case class FastFactors(var arr: Array[FastFactor])

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
object Sieve {
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

case class Sieve(start: N, primes: BitSet, cutoff: N) {
  def isPrime(n: N): Boolean = primes((n - start).toInt)
  def isComposite(n: N): Boolean = !primes((n - start).toInt)
  def set(n: N): Unit = primes += (n - start).toInt
  def unset(n: N): Unit = primes -= (n - start).toInt

  def nextAfter(n: N): N = {
    var i = (n - start + 2).toInt
    val len = primes.length
    while (i < len) {
      if (primes(i)) return start + i
      i += 2
    }
    N(-1L) // fail
  }

  //def init(fastq: FastFactors, slowq: PriorityQueue[Factor]) {
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
    val arr = primes.arr
    assert(arr.length % 15 == 0)
    val limit = arr.length
    val wheel = Sieve.wheel30
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

    val len: Int = if (start + primes.length < cutoff)
      (cutoff - start).toInt
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

  //@tailrec private def initFromQueue(limit: N, q: PriorityQueue[Factor]) {
  @tailrec private def initFromQueue(limit: N, q: FactorHeap) {
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

  //def initFirst(fastq: FastFactors, slowq: PriorityQueue[Factor]) {
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
          buf += FastFactor(p, N(m))
        } else if (p < cutoff) {
          slowq += Factor(N(p), N(m))
        }
      }
      p += 2
    }
    fastq.arr = buf.toArray
  }

  //def initRest(slowq: PriorityQueue[Factor]) {
  def initRest(slowq: FactorHeap) {
    if (start >= cutoff) return ()

    val len: Int = if (start + primes.length >= cutoff)
      (cutoff - start).toInt
    else
      primes.length

    var i = 1
    while (i < len) {
      if (primes(i)) {
        val p: N = start + i
        slowq += Factor(p, p ** 2)
      }
      i += 2
    }
  }
}

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
object Siever {
  val sieveSize = 9600 * 1000

  def nth(n: Int): N = {
    val upper = n * log(n) + n * log(log(n - 0.9385))
    val cutoff = max(1000L, (scala.math.sqrt(upper) + 512L).toLong)
    Siever(sieveSize, cutoff).nth(n)
  }
}

case class Siever(chunkSize: Int, cutoff: N) {
  if (chunkSize % 480 != 0) sys.error("chunkSize must be a multiple of 480")

  val arr = BitSet.alloc(chunkSize)
  var start: N = N(0)
  var limit: N = start + chunkSize
  val fastq: FastFactors = FastFactors.empty
  //val slowq: PriorityQueue[Factor] = PriorityQueue.empty[Factor]
  val slowq: FactorHeap = new FactorHeap
  var sieve: Sieve = Sieve(start, arr, cutoff)
  sieve.init(fastq, slowq)

  def largestBelow(n: N): N = {
    if (n < 3) sys.error("invalid argument: %s" format n)
    if (n == 3) return N(2)

    var i = 3
    var k = n - 1
    var last = N(2)
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
    return N(0) // impossible
  }

  def nth(n: Int): N = {
    if (n == 1) return N(2)
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
    return N(0) // impossible
  }

  private def initNextSieve() {
    start += chunkSize
    limit += chunkSize
    val csq = cutoff ** 2
    if (limit >= csq) sys.error("too big: %s > %s (%s)" format (limit, csq, cutoff))
    arr.clear()
    sieve = Sieve(start, arr, cutoff)
    sieve.init(fastq, slowq)
  }

  def nextAfter(n: N): N = {
    var nn = sieve.nextAfter(n)
    while (nn == -1L) {
      initNextSieve()
      nn = sieve.nextAfter(start - 1)
    }
    nn
  }
}

object SieveTiming {
  val cutoff = N(1000 * 1000) // max prime we can find will be <1T

  def main(args: Array[String]) {
    //test(args)
    run(args)
  }

  def test(args: Array[String]) {
    def find1(n: Int) = Siever(480, cutoff).nth(n)
    def find2(n: Int) = Siever(480 * 10, cutoff).nth(n)

    def check(n: Int): Boolean = {
      val x1 = find1(n)
      val x2 = find2(n)
      if (x1 == x2) {
        println("ok for %s: got %s" format (n, x1))
        true
      } else {
        println("error for %s: got %s expected %s" format (n, x1, x2))
        false
      }
    }

    //check(1000000)
    //check(79831)
    println(find2(1 * 1000 * 1000))
    // println(find2(79832))
    // println(find2(79833))
  }

  def run(args: Array[String]) {
    val ns = (
      1 ::
      10 ::
      100 ::
      1 * 1000 ::
      10 * 1000 ::
      100 * 1000 ::
      1 * 1000 * 1000 ::
      10 * 1000 * 1000 ::
      100 * 1000 * 1000 ::
      1 * 1000 * 1000 * 1000 ::
      Int.MaxValue ::
      Nil
    )

    println("warming up the sieve...")
    ns.take(8).foreach(n => Siever.nth(n))
    println("done")

    System.gc()
    Thread.sleep(1000)

    println("timing the sieve...")
    ns.foreach { n =>
      timer("  siever.nth (%s)" format n) {
        Siever.nth(n)
      }
    }
  }

  def timer[A](s: String)(f: => A): A = {
    val t0 = System.nanoTime
    val a = f
    val t = System.nanoTime - t0
    println("%s generated %s in %.1f ms" format (s, a, t / 1000000.0))
    a
  }
}
