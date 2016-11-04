package spire
package math

import spire.algebra.Sign
import spire.algebra.Sign.Positive
import spire.syntax.cfor._
import spire.syntax.nroot._

import scala.collection.mutable

/**
 * Basic tools for prime factorization.
 *
 * This package is intended to provide tools for factoring numbers,
 * checking primality, generating prime numbers, etc. For now, its
 * main contributions are a method for factoring integers
 * (spire.math.prime.factor) and a type for representing prime factors
 * and their exponents (spire.math.prime.Factors).
 *
 * The factorization currently happens via an implementation of
 * Pollard-Rho with Brent's optimization. This technique works very
 * well for composites with small prime factors (up to 10 decimal
 * digits or so) and can support semiprimes (products of two
 * similarly-sized primes) of 20-40 digits.
 *
 * The implementation does cheat, using BigInteger.isProbablePrime(40)
 * to test basic primality. This has a roughly 1-in-1,000,000,000,000
 * chance of being wrong.
 *
 * Since Pollard-Rho uses random primes, its performance is somewhat
 * non-deterministic. On this machine, factoring 20-digit semiprimes
 * seem to average about 1.5s and factoring 30-digit semiprimes seem
 * to average about 20s. Much larger numbers can be factored provided
 * they are either prime or composites with smallish factors.
 */
package object prime {

  /**
   * Determine if the given integer is prime.
   *
   * Currently this is using a strong pseudo-primality test (so there
   * is a 1-in-1,000,000,000,000 chance of being wrong).
   */
  def isPrime(n: SafeLong): Boolean = n.isProbablePrime(40)

  /**
   * Factor the given integer with the default factorization method.
   */
  def factor(n: SafeLong): Factors = factorPollardRho(n)

  /**
   * Factor the given integer using trial division.
   *
   * This is the slowest method, but is still reasonable for numbers
   * up to about 14 decimal digits or so.
   */
  def factorTrialDivision(n0: SafeLong): Factors = {
    if (n0 == 0) return Factors.zero

    val n = n0.abs
    val sign = Sign(n0.signum)
    if (n == SafeLong.one) return Factors(Map.empty, sign)

    val facts = mutable.Map.empty[SafeLong, Int]
    var x = n
    val (x1, e1) = findPowers(x, SafeLong(2))
    if (e1 > 0) {
      facts(SafeLong(2)) = e1
      x = x1
    }

    var limit = x.sqrt
    cfor(SafeLong(3))(_ <= limit && x > 1, _ + 2) { b =>
      val (x2, e2) = findPowers(x, b)
      if (e2 > 0) {
        facts(b) = e2
        x = x2
        limit = x.sqrt
      }
    }
    if (x > 1) facts(x) = 1
    Factors(facts.toMap, sign)
  }

  /**
   * Factor the given integer using trial division with a wheel.
   *
   * This is slightly faster than basic trial divison (about 30% or
   * so). It's still mostly appropriate for small-ish numbers.
   */
  def factorWheelDivision(n0: SafeLong): Factors = {
    if (n0 == 0) return Factors.zero

    val n = n0.abs
    val sign = Sign(n0.signum)
    if (n == 1) return Factors(Map.empty, sign)

    val facts = mutable.Map.empty[SafeLong, Int]
    var x = n
    val (x1, e1) = findPowers(x, SafeLong(2))
    if (e1 > 0) {
      facts(SafeLong(2)) = e1
      x = x1
    }

    cfor(SafeLong(3))(_ < 30 && x > 1, _ + 2) { b =>
      val (x2, e2) = findPowers(x, b)
      if (e2 > 0) {
        facts(b) = e2
        x = x2
      }
    }

    var limit = x.sqrt
    var b = SafeLong(31)
    var i = 0
    val offsets = Array(2, 2, 2, 4, 2, 4, 2, 4, 6, 2)
    while (b <= limit && x > 1) {
      val (x2, e2) = findPowers(x, b)
      if (e2 > 0) {
        facts(b) = e2
        x = x2
        limit = x.sqrt
      }
      b += offsets(i)
      i = (i + 1) % 10
    }

    if (x > 1) facts(x) = 1
    Factors(facts.toMap, sign)
  }

  def factorPollardRho(n0: SafeLong): Factors = {

    def rho(n: SafeLong, c: SafeLong): SafeLong = {

      @inline def f(x: SafeLong): SafeLong = ((x * x) tmod n + c) tmod n

      @tailrec def fastRho(x: SafeLong, q0: SafeLong, r: SafeLong, m: SafeLong): SafeLong = {
        var y = x
        var q = q0
        cfor(0)(r > _, _ + 1)(_ => y = f(y))

        var g = SafeLong.one
        var k = SafeLong.zero
        var ys = y
        while (r > k && g == 1) {
          ys = y
          val limit = m min (r - k)
          cfor(0)(limit > _, _ + 1) { _ =>
            y = f(y)
            q = (q * (x - y).abs) tmod n
          }
          if (q == 0) g = n else g = n gcd q
          k = k + m
        }

        if (g == 1) fastRho(y, q, r * 2, m) else if (g == n) slowRho(x, ys) else g
      }

      @tailrec def slowRho(x: SafeLong, ys: SafeLong): SafeLong = {
        val yys = f(ys)
        val g = n gcd (x - yys).abs
        if (g == 1) slowRho(x, yys) else g
      }

      fastRho(rand(n), SafeLong.one, SafeLong.one, rand(n))
    }

    def factor(n: SafeLong): Factors = {
      if (n == 1) {
        Factors.one
      } else if (isPrime(n)) {
        Factors(Map((n, 1)), Positive)
      } else if (n.isEven) {
        var x = n / 2
        var e = 1
        while (x.isEven) { x /= 2; e += 1 }
        Factors(Map((SafeLong(2), e)), Positive) * factor(x)
      } else {
        var divisor = rho(n, rand(n))
        while (divisor == n) divisor = rho(n, rand(n))
        factor(divisor) * factor(n / divisor)
      }
    }

    if (n0 == 0) return Factors.zero

    val n = n0.abs
    if (n == 1) return Factors(Map.empty, Sign(n0.signum))
    if (n0 < 0) -factor(n) else factor(n)
  }

  private val srand = new java.util.Random

  private def rand(n: SafeLong): SafeLong = {
    val bits = n.bitLength
    var x = new java.math.BigInteger(bits, srand)
    while (x.signum == 0) x = new java.math.BigInteger(bits, srand)
    SafeLong(x)
  }

  private def findPowers(x0: SafeLong, b: SafeLong): (SafeLong, Int) = {
    var x = x0
    var e = 0
    while (x > 1 && (x tmod b) == 0) { e += 1; x = x / b }
    (x, e)
  }

  private val SieveSize = 9600 * 1000

  def sieverUpToNth(n: Long): Siever = {
    val upper = n * log(n) + n * log(log(n - 0.9385))
    val cutoff = max(1000L, (sqrt(upper) + 512L).toLong)
    prime.Siever(SieveSize, cutoff)
  }

  def nth(n: Long): SafeLong =
    sieverUpToNth(n).nth(n)

  import SafeLong.{two, three}

  def fill(n: Int): Array[SafeLong] = {
    if (n <= 0) throw new IllegalArgumentException(n.toString)
    else if (n == 1) Array(two)
    else {
      val siever = sieverUpToNth(n)
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

  def fill(start: Int, limit: Int): Array[SafeLong] =
    if (start == 0) fill(limit) else {
      val siever = sieverUpToNth(start + limit)
      def loop(i: Int, p: SafeLong): Array[SafeLong] =
        if (i < start) loop(i + 1, siever.nextAfter(p))
        else siever.arrayAt(p, limit)
      loop(1, three)
    }

  def stream: Stream[SafeLong] =
    stream(SieveSize, SafeLong(1000000))

  def stream(chunkSize: Int, cutoff: SafeLong): Stream[SafeLong] =
    two #:: three #:: prime.Siever(chunkSize, cutoff).streamAfter(three)
}
