package spire
package math.prime

import spire.algebra.{Sign, UniqueFactorizationDomain}
import spire.algebra.Sign.{Negative, Positive, Zero}
import spire.math.SafeLong
import spire.std.int._
import spire.std.map._
import spire.syntax.rng._

object Factors {
  val zero = Factors(Map.empty, Zero)
  val one = Factors(Map.empty, Positive)

  def apply(n: Long): Factors = factor(SafeLong(n))
  def apply(n: BigInt): Factors = factor(SafeLong(n))
  def apply(n: SafeLong): Factors = factor(n)
  def apply(s: String): Factors = factor(SafeLong(s))
}

case class Factors(elements: Map[SafeLong, Int], sign: Sign)
    extends UniqueFactorizationDomain.Decomposition[SafeLong]
    with Iterable[(SafeLong, Int)]
    with Ordered[Factors] { lhs =>

  def unit: SafeLong = SafeLong(sign.toInt)

  private[prime] def prod(m: Map[SafeLong, Int]): SafeLong =
    m.foldLeft(SafeLong.one) { case (t, (p, e)) => t * p.pow(e) }

  lazy val value: SafeLong = sign match {
    case Positive => prod(elements)
    case Zero     => SafeLong.zero
    case Negative => -prod(elements)
  }

  override def toString(): String = {
    def terms =
      if (elements.isEmpty) "1"
      else elements.toSeq.sorted.map { case (p, e) => s"$p^$e" }.mkString(" * ")
    sign match {
      case Positive => s"($terms)"
      case Zero     => "(0)"
      case Negative => s"-($terms)"
    }
  }

  def signum: Int = sign.toInt

  def iterator: Iterator[(SafeLong, Int)] = elements.iterator

  def toMap: Map[SafeLong, Int] = elements

  def uniqueFactors: Set[SafeLong] = elements.keySet

  def contains(p: SafeLong): Boolean = elements.contains(p)

  def get(p: SafeLong): Int = elements.getOrElse(p, 0)

  def compare(rhs: Factors): Int = {
    val n = lhs.signum - rhs.signum
    if (n == 0) lhs.value.compare(rhs.value) else java.lang.Integer.signum(n)
  }

  def compare(rhs: Int): Int =
    sign match {
      case Positive =>
        var t = SafeLong.one
        val it = iterator
        while (it.hasNext && t <= rhs) { val (p, e) = it.next(); t *= (p ** e) }
        t.compare(rhs)
      case Zero =>
        rhs.sign.toInt
      case Negative =>
        var t = -SafeLong.one
        val it = iterator
        while (it.hasNext && t >= rhs) { val (p, e) = it.next(); t *= (p ** e) }
        t.compare(rhs)
    }

  def gcd(rhs: Factors): Factors =
    Factors(lhs.elements.flatMap { case (p, le) =>
              rhs.elements.get(p).map(re => (p, le.min(re)))
            },
            Positive
    )

  def lcm(rhs: Factors): Factors =
    Factors(lhs.elements.foldLeft(rhs.elements) { case (fs, (p, e)) =>
              fs.updated(p, fs.getOrElse(p, 0).max(e))
            },
            Positive
    )

  def unary_- : Factors = Factors(elements, -sign)

  def +(rhs: Factors): Factors = Factors(lhs.value + rhs.value)
  def +(rhs: SafeLong): Factors = Factors(lhs.value + rhs)

  def -(rhs: Factors): Factors = Factors(lhs.value - rhs.value)
  def -(rhs: SafeLong): Factors = Factors(lhs.value - rhs)

  def *(rhs: Factors): Factors =
    Factors(lhs.elements + rhs.elements, lhs.sign * rhs.sign)
  def *(rhs: SafeLong): Factors =
    lhs * Factors(rhs)

  private[prime] def qm(rhs: Factors): (Int, Map[SafeLong, Int], Map[SafeLong, Int], Map[SafeLong, Int]) = {
    val sign = (lhs.sign * rhs.sign).toInt
    val (nn, dd) = (lhs.elements - rhs.elements).filter(_._2 != 0).partition(_._2 > 0)
    val cc = lhs.elements.flatMap { case (p, le) =>
      rhs.elements.get(p).iterator.map(re => (p, le.min(re)))
    }
    (sign, nn, dd.map { case (p, e) => (p, -e) }, cc)
  }

  def /(rhs: Factors): Factors = {
    val (sign, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) Factors(nn, sign) else Factors((prod(nn) * sign) / prod(dd))
  }

  def /(rhs: SafeLong): Factors =
    elements.get(rhs) match {
      case Some(1) =>
        Factors(elements - rhs, sign)
      case Some(n) =>
        Factors(elements.updated(rhs, n - 1), sign)
      case None =>
        val n = lhs.value / rhs
        if (n < rhs) Factors(n) else lhs / Factors(rhs)
    }

  def %(rhs: Factors): Factors = {
    val (_, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) Factors.zero
    else Factors(((prod(nn) * lhs.signum) % prod(dd)) * prod(cc))
  }

  def %(rhs: SafeLong): Factors =
    lhs % Factors(rhs)

  def /%(rhs: Factors): (Factors, Factors) = {
    val (sign, nn, dd, cc) = qm(rhs)
    if (dd.isEmpty) {
      (Factors(nn, sign), Factors.zero)
    } else {
      val (q, m) = prod(nn) /% prod(dd)
      (Factors(q) * sign, Factors(m * prod(cc)) * lhs.signum)
    }
  }

  def /%(rhs: SafeLong): (Factors, Factors) =
    elements.get(rhs) match {
      case Some(1) =>
        (Factors(elements - rhs, sign), Factors.zero)
      case Some(n) =>
        (Factors(elements.updated(rhs, n - 1), sign), Factors.zero)
      case None =>
        val (q, m) = lhs.value /% rhs
        (Factors(q), Factors(m))
    }

  def pow(rhs: Int): Factors =
    if (rhs < 0) {
      throw new IllegalArgumentException("negative exponent")
    } else if (rhs == 0) {
      Factors.one
    } else {
      val sign = lhs.sign match {
        case Negative if (rhs & 1) == 0 => Positive
        case sign                       => sign
      }
      Factors(lhs.elements.map { case (p, e) => (p, e * rhs) }, sign)
    }
}
