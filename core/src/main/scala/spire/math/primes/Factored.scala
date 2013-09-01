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

import scala.collection.mutable

object Factored {
  val zero = Factored(Map.empty, Zero)
  val one = Factored(Map.empty, Positive)

  def findPowers(x0: SafeLong, b: SafeLong): (SafeLong, Int) = {
    var x = x0
    var e = 0
    while (x > 1 && x % b == 0) { e += 1; x = x / b }
    (x, e)
  }

  def trialDivision(n0: SafeLong): Factored = {
    if (n0 == 0) return zero

    val n = n0.abs
    val sign = Sign(n0.signum)
    if (n == SafeLong.one) return Factored(Map.empty, sign)

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
    Factored(facts.toMap, sign)
  }

  def wheelDivision(n0: SafeLong): Factored = {
    if (n0 == 0) return zero

    val n = n0.abs
    val sign = Sign(n0.signum)
    if (n == 1) return Factored(Map.empty, sign)

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
    Factored(facts.toMap, sign)
  }

  private val srand = new java.security.SecureRandom

  def rand(n: SafeLong): SafeLong =
    SafeLong(new java.math.BigInteger(n.fold(_ => 64, _.bitLength), srand))

  def isPrime(n: SafeLong): Boolean =
    n.toBigInt.isProbablePrime(20)

  def pollardRho(n0: SafeLong): Factored = {

    def rho(n: SafeLong): SafeLong = {
      val c = rand(n)
      def f(x: SafeLong): SafeLong = ((x * x) % n + c) % n
      def rhoLoop(x: SafeLong, y: SafeLong): SafeLong = {
        val x2 = f(x)
        val y2 = f(f(y))
        val d = (x2 - y2) gcd n
        if (d == 1) rhoLoop(x2, y2) else d
      }
      val x = rand(n)
      rhoLoop(x, x)
    }

    def fastRho(n: SafeLong): SafeLong = {
      val c = rand(n)
      def f(x: SafeLong): SafeLong = ((x * x) % n + c) % n
      def fastRhoLoop(x: SafeLong, y: SafeLong): SafeLong = {
        var i = 0
        var x2 = f(x)
        var y2 = f(f(y))
        var z = (x2 - y2)
        while (i < 100) {
          i += 1
          x2 = f(x2)
          y2 = f(f(y2))
          z = (z * (x2 - y2)) % n
        }
        val d = z gcd n
        if (d == 1) fastRhoLoop(x2, y2)
        else if (d == n) rhoLoop(x, y)
        else d
      }
      def rhoLoop(x: SafeLong, y: SafeLong): SafeLong = {
        val x2 = f(x)
        val y2 = f(f(y))
        val d = (x2 - y2) gcd n
        if (d == 1) rhoLoop(x2, y2) else d
      }
      val x = rand(n)
      fastRhoLoop(x, x)
    }

    def factor(n: SafeLong): Factored = {
      if (n == 1) {
        Factored.one
      } else if (isPrime(n)) {
        Factored(Map(n -> 1), Positive)
      } else if (n % 2 == 0) {
        var x = n / 2
        var e = 1
        while (x % 2 == 0) { x /= 2; e += 1 }
        Factored(Map(SafeLong(2) -> e), Positive) * factor(x)
      } else {
        // TODO: alternately, use the simpler pollard-rho algorithm
        // by calling rho(n) instead
        var divisor = fastRho(n)
        while (divisor == n) divisor = fastRho(n)
        factor(divisor) * factor(n / divisor)
      }
    }

    if (n0 == 0) return zero

    val n = n0.abs
    if (n == 1) return Factored(Map.empty, Sign(n0.signum))
    if (n0 < 0) -factor(n) else factor(n)
  }

  def apply(n: SafeLong): Factored = pollardRho(n)
}

case class Factored(factors: Map[SafeLong, Int], sign: Sign) extends Ordered[Factored] { lhs =>
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

  def compare(rhs: Factored): Int =
    lhs.value compare rhs.value

  def unary_-(): Factored =
    Factored(factors, -sign)

  def +(rhs: Factored): Factored =
    Factored(lhs.value + rhs.value)

  def -(rhs: Factored): Factored =
    Factored(lhs.value - rhs.value)

  def *(rhs: Factored): Factored =
    Factored(lhs.factors + rhs.factors, lhs.sign * rhs.sign)

  private[primes] def qm(rhs: Factored): (SafeLong, SafeLong) = {
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

  def /(rhs: Factored): Factored =
    Factored(this.qm(rhs)._1)

  def %(rhs: Factored): Factored =
    Factored(this.qm(rhs)._2)

  def /%(rhs: Factored): (Factored, Factored) = {
    val (q, r) = this.qm(rhs)
    (Factored(q), Factored(r))
  }

  def pow(rhs: Int): Factored =
    Factored(lhs.factors.map { case (p, e) => (p, e ** rhs) }, lhs.sign ** rhs)
}
