package spire.math.prime

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

  // TODO: 1-in-1M chance of being wrong. too risky?
  // maybe we should use 30, i.e. 1-in-1B?
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

  def apply(s: String): Factors = pollardRho(SafeLong(s))
}

case class Factors(factors: Map[SafeLong, Int], sign: Sign) extends Iterable[(SafeLong, Int)] with Ordered[Factors] { lhs =>

  private[prime] def prod(m: Map[SafeLong, Int]): SafeLong =
    m.foldLeft(SafeLong.one) { case (t, (p, e)) => t * (p ** e) }

  lazy val value: SafeLong = sign match {
    case Positive => prod(factors)
    case Zero => SafeLong.zero
    case Negative => -prod(factors)
  }

  override def toString(): String = {
    def terms =
      if (factors.isEmpty) "1"
      else factors.toSeq.sorted.map { case (p, e) => s"$p^$e" }.mkString(" * ")
    sign match {
      case Positive => s"($terms)"
      case Zero => "(0)"
      case Negative => s"-($terms)"
    }
  }

  def iterator: Iterator[(SafeLong, Int)] =
    factors.iterator

  def uniqueFactors: Set[SafeLong] =
    factors.keySet

  def contains(p: SafeLong): Boolean =
    factors.contains(p)

  def get(p: SafeLong): Int =
    factors.getOrElse(p, 0)

  def compare(rhs: Factors): Int =
    lhs.value compare rhs.value

  def compare(rhs: Int): Int =
    sign match {
      case Positive =>
        var t = SafeLong.one
        val it = iterator
        while (it.hasNext && t <= rhs) { val (p, e) = it.next(); t *= (p ** e) }
        t compare rhs
      case Zero =>
        rhs.signum
      case Negative =>
        var t = -SafeLong.one
        val it = iterator
        while (it.hasNext && t >= rhs) { val (p, e) = it.next(); t *= (p ** e) }
        t compare rhs
    }

  def gcd(rhs: Factors): Factors =
    Factors(lhs.factors.flatMap { case (p, le) =>
      rhs.factors.get(p).map(re => (p, le min re))
    }, Positive)

  def lcm(rhs: Factors): Factors =
    Factors(lhs.factors.foldLeft(rhs.factors) { case (fs, (p, e)) =>
      fs.updated(p, fs.getOrElse(p, 0) max e)
    }, Positive)

  def unary_-(): Factors =
    Factors(factors, -sign)

  def +(rhs: Factors): Factors =
    Factors(lhs.value + rhs.value)
  def +(rhs: SafeLong): Factors =
    Factors(lhs.value + rhs)

  def -(rhs: Factors): Factors =
    Factors(lhs.value - rhs.value)
  def -(rhs: SafeLong): Factors =
    Factors(lhs.value - rhs)

  def *(rhs: Factors): Factors =
    Factors(lhs.factors + rhs.factors, lhs.sign * rhs.sign)
  def *(rhs: SafeLong): Factors =
    Factors(factors.updated(rhs, factors.getOrElse(rhs, 0) + 1), sign)

  private[prime] def qm(rhs: Factors) = {
    val sign = (lhs.sign * rhs.sign).toInt
    val (nn, dd) = (lhs.factors - rhs.factors).filter(_._2 != 0).partition(_._2 > 0)
    val cc = lhs.factors.flatMap { case (p, le) =>
      rhs.factors.get(p).map(re => (p, le min re))
    }
    (sign, nn, dd.map { case (p, e) => (p, -e) }, cc)
  }

  def /(rhs: Factors): Factors = {
    val (sign, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) Factors(nn, sign)
    else Factors((prod(nn) * sign) / prod(dd))
  }

  def /(rhs: SafeLong): Factors =
    factors.get(rhs) match {
      case Some(1) => Factors(factors - rhs, sign)
      case Some(n) => Factors(factors.updated(rhs, n - 1), sign)
      case None => Factors(lhs.value / rhs)
    }

  def %(rhs: Factors): Factors = {
    val (sign, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) Factors.zero
    else Factors(((prod(nn) * sign) % prod(dd)) * prod(cc))
  }

  def %(rhs: SafeLong): Factors =
    if (factors.contains(rhs)) Factors.zero else Factors(lhs.value % rhs)

  def /%(rhs: Factors): (Factors, Factors) = {
    val (sign, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) {
      (Factors(nn, sign), Factors.zero)
    } else {
      val (q, m) = (prod(nn) * sign) /% prod(dd)
      (Factors(q), Factors(m * prod(cc)))
    }
  }

  def /%(rhs: SafeLong): (Factors, Factors) =
    factors.get(rhs) match {
      case Some(1) =>
        (Factors(factors - rhs, sign), Factors.zero)
      case Some(n) =>
        (Factors(factors.updated(rhs, n - 1), sign), Factors.zero)
      case None =>
        val (q, m) = lhs.value /% rhs
        (Factors(q), Factors(m))
    }

  def pow(rhs: Int): Factors =
    Factors(lhs.factors.map { case (p, e) => (p, e ** rhs) }, lhs.sign ** rhs)
}
