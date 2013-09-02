package spire.math.primes

import spire.algebra.Sign
import spire.algebra.Sign.{Negative, Zero, Positive}
import spire.math.SafeLong
import spire.std.int._
import spire.std.map._
import spire.syntax.cfor._
import spire.syntax.std.seq._
import spire.syntax.nroot._
import spire.syntax.rng._

import scala.annotation.tailrec
import scala.collection.mutable

object Factors {
  val zero = Factors(Map.empty, Zero)
  val one = Factors(Map.empty, Positive)

  def findPowers(x0: SafeLong, b: SafeLong): (SafeLong, Int) = {
    var x = x0
    var e = 0
    while (x > 1 && x % b == 0) { e += 1; x = x / b }
    (x, e)
  }

  def trialDivision(n0: SafeLong): Factors = {
    if (n0 == 0) return zero

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

  def wheelDivision(n0: SafeLong): Factors = {
    if (n0 == 0) return zero

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

  private val srand = new java.security.SecureRandom

  def rand(n: SafeLong): SafeLong = {
    val bits = n.fold(_ => 64, _.bitLength)
    var x = new java.math.BigInteger(bits, srand)
    while (x == 0) x = new java.math.BigInteger(bits, srand)
    SafeLong(x)
  }

  def isPrime(n: SafeLong): Boolean =
    n.toBigInt.isProbablePrime(20)

  def pollardRho(n0: SafeLong): Factors = {

    def rho(n: SafeLong, c: SafeLong): SafeLong = {

      @inline def f(x: SafeLong): SafeLong = ((x * x) % n + c) % n

      @tailrec def fastRho(x: SafeLong, q0: SafeLong, r: SafeLong, m :SafeLong): SafeLong = {
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
            q = (q * (x - y).abs) % n
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
        Factors(Map(n -> 1), Positive)
      } else if (n % 2 == 0) {
        var x = n / 2
        var e = 1
        while (x % 2 == 0) { x /= 2; e += 1 }
        Factors(Map(SafeLong(2) -> e), Positive) * factor(x)
      } else {
        var divisor = rho(n, rand(n))
        while (divisor == n) divisor = rho(n, rand(n))
        factor(divisor) * factor(n / divisor)
      }
    }

    if (n0 == 0) return zero

    val n = n0.abs
    if (n == 1) return Factors(Map.empty, Sign(n0.signum))
    if (n0 < 0) -factor(n) else factor(n)
  }

  def apply(n: SafeLong): Factors = pollardRho(n)
}

case class Factors(factors: Map[SafeLong, Int], sign: Sign) extends Ordered[Factors] { lhs =>
  lazy val value: SafeLong = sign match {
    case Positive => factors.map { case (p, e) => p ** e }.qproduct
    case Zero => SafeLong.zero
    case Negative => -factors.map { case (p, e) => p ** e }.qproduct
  }

  override def toString(): String = {
    def terms =
      if (factors.isEmpty) "(1)"
      else factors.toSeq.sorted.map { case (p, e) => s"$p^$e" }.mkString(" * ")
    sign match {
      case Positive => s"($terms)"
      case Zero => "(0)"
      case Negative => s"-($terms)"
    }
  }

  def compare(rhs: Factors): Int =
    lhs.value compare rhs.value

  def unary_-(): Factors =
    Factors(factors, -sign)

  def +(rhs: Factors): Factors =
    Factors(lhs.value + rhs.value)

  def -(rhs: Factors): Factors =
    Factors(lhs.value - rhs.value)

  def *(rhs: Factors): Factors =
    Factors(lhs.factors + rhs.factors, lhs.sign * rhs.sign)

  private[primes] def qm(rhs: Factors): (SafeLong, SafeLong) = {
    var common = SafeLong.zero
    var num = SafeLong.zero
    var denom = SafeLong.zero

    val sign = (lhs.sign * rhs.sign).toInt
    val nn = mutable.Map.empty[SafeLong, Int] ++ lhs.factors
    val dd = mutable.Map.empty[SafeLong, Int] ++ rhs.factors
    val facts = if (lhs.factors.size < rhs.factors.size) lhs.factors else rhs.factors

    facts.keys.foreach { p =>
      val ne = nn.getOrElse(p, 0)
      val de = dd.getOrElse(p, 0)
      if (ne > 0) {
        val e = ne min de
        nn(p) -= e
        dd(p) -= e
        common += p ** e
      }
    }

    if (dd.isEmpty) {
      val q = nn.map { case (p, e) => p ** e }.qproduct
      (q, SafeLong.zero)
    } else {
      (nn.toMap - dd.toMap).foreach { case (p, e) =>
        if (e > 0) num += p ** e
        else if (e < 0) denom += p ** e
      }
      val (q, m) = (num * sign) /% denom
      (q, m * common)
    }
  }

  def /(rhs: Factors): Factors =
    Factors(this.qm(rhs)._1)

  def %(rhs: Factors): Factors =
    Factors(this.qm(rhs)._2)

  def /%(rhs: Factors): (Factors, Factors) = {
    val (q, r) = this.qm(rhs)
    (Factors(q), Factors(r))
  }

  def pow(rhs: Int): Factors =
    Factors(lhs.factors.map { case (p, e) => (p, e ** rhs) }, lhs.sign ** rhs)
}
