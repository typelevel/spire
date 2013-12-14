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

  def apply(n: Long): Factors = factor(SafeLong(n))
  def apply(n: BigInt): Factors = factor(SafeLong(n))
  def apply(n: SafeLong): Factors = factor(n)
  def apply(s: String): Factors = factor(SafeLong(s))
}

case class Factors(factors: Map[SafeLong, Int], sign: Sign)
    extends Iterable[(SafeLong, Int)] with Ordered[Factors] { lhs =>

  private[prime] def prod(m: Map[SafeLong, Int]): SafeLong =
    m.foldLeft(SafeLong.one) { case (t, (p, e)) => t * p.pow(e) }

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

  def signum: Int = sign.toInt

  def iterator: Iterator[(SafeLong, Int)] = factors.iterator

  def toMap: Map[SafeLong, Int] = factors

  def uniqueFactors: Set[SafeLong] = factors.keySet

  def contains(p: SafeLong): Boolean = factors.contains(p)

  def get(p: SafeLong): Int = factors.getOrElse(p, 0)

  def compare(rhs: Factors): Int = lhs.value compare rhs.value

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

  def unary_-(): Factors = Factors(factors, -sign)

  def +(rhs: Factors): Factors = Factors(lhs.value + rhs.value)
  def +(rhs: SafeLong): Factors = Factors(lhs.value + rhs)

  def -(rhs: Factors): Factors = Factors(lhs.value - rhs.value)
  def -(rhs: SafeLong): Factors = Factors(lhs.value - rhs)

  def *(rhs: Factors): Factors =
    Factors(lhs.factors + rhs.factors, lhs.sign * rhs.sign)
  def *(rhs: SafeLong): Factors =
    Factors(factors.updated(rhs, factors.getOrElse(rhs, 0) + 1), sign)

  private[prime] def qm(rhs: Factors): (Int, Map[SafeLong, Int], Map[SafeLong, Int], Map[SafeLong, Int]) = {
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
    if (rhs < 0) {
      throw new IllegalArgumentException("negative exponent")
    } else if (rhs == 0) {
      Factors.one
    } else {
      val sign = lhs.sign match {
        case Negative if (rhs & 1) == 0 => Positive
        case sign => sign
      }
      Factors(lhs.factors.map { case (p, e) => (p, e * rhs) }, sign)
    }
}
