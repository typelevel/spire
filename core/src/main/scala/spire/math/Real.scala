/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import scala.collection.immutable.LazyList
import scala.collection.immutable.LazyList.#::

import spire.algebra.{Field, Order, Trig, TruncatedDivisionCRing}
import spire.syntax.nroot._
import spire.util.Opt
import scala.annotation.nowarn

sealed trait Real extends ScalaNumber with ScalaNumericConversions { x =>

  import Real.{roundUp, Exact}

  def apply(p: Int): SafeLong

  def toRational(p: Int): Rational = this match {
    case Exact(n) => n
    case _        => Rational(x(p), SafeLong.two.pow(p))
  }

  def toRational: Rational = toRational(Real.bits)

  // ugh scala.math
  override def doubleValue: Double = toRational.toDouble
  override def floatValue: Float = toRational.toFloat
  override def intValue: Int = toRational.toInt
  override def longValue: Long = toRational.toLong
  override def underlying: Object = this

  override def isValidChar: Boolean = {
    val r = toRational
    r.isWhole && r.isValidChar
  }

  override def isValidByte: Boolean = {
    val r = toRational
    r.isWhole && r.isValidByte
  }

  override def isValidShort: Boolean = {
    val r = toRational
    r.isWhole && r.isValidShort
  }

  override def isValidInt: Boolean = {
    val r = toRational
    r.isWhole && r.isValidInt
  }

  def isValidLong: Boolean = {
    val r = toRational
    r.isWhole && r.isValidLong
  }

  override def hashCode(): Int = toRational.hashCode

  @nowarn
  override def equals(y: Any): Boolean = y match {
    case y: Real => this === y
    case y       => toRational.equals(y)
  }

  def ===(y: Real): Boolean =
    x.compare(y) == 0

  def =!=(y: Real): Boolean =
    !(this === y)

  def compare(y: Real): Int = (x, y) match {
    case (Exact(nx), Exact(ny)) => nx.compare(ny)
    case _                      => (x - y).signum
  }

  def min(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx.min(ny))
    case _                      => Real(p => x(p).min(y(p)))
  }

  def max(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx.max(ny))
    case _                      => Real(p => x(p).max(y(p)))
  }

  def abs: Real = this match {
    case Exact(n) => Exact(n.abs)
    case _        => Real(p => x(p).abs)
  }

  def signum: Int = this match {
    case Exact(n) => n.signum
    case _        => x(Real.bits).signum
  }

  def unary_- : Real = this match {
    case Exact(n) => Exact(-n)
    case _        => Real(p => -x(p))
  }

  def reciprocal: Real = {
    def findNonzero(i: Int): Int =
      if (SafeLong.three <= x(i).abs) i else findNonzero(i + 1)

    this match {
      case Exact(n) => Exact(n.reciprocal)
      case _ =>
        Real { p =>
          val s = findNonzero(0)
          roundUp(Rational(SafeLong.two.pow(2 * p + 2 * s + 2), x(p + 2 * s + 2)))
        }
    }
  }

  def +(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny))    => Exact(nx + ny)
    case (Exact(Rational.zero), _) => y
    case (_, Exact(Rational.zero)) => x
    case _                         => Real(p => roundUp(Rational(x(p + 2) + y(p + 2), 4)))
  }

  def -(y: Real): Real = x + -y

  def *(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny))    => Exact(nx * ny)
    case (Exact(Rational.zero), _) => Real.zero
    case (_, Exact(Rational.zero)) => Real.zero
    case (Exact(Rational.one), _)  => y
    case (_, Exact(Rational.one))  => x
    case _ =>
      Real { p =>
        val x0 = x(0).abs + 2
        val y0 = y(0).abs + 2
        val sx = Real.sizeInBase(x0, 2) + 3
        val sy = Real.sizeInBase(y0, 2) + 3
        roundUp(Rational(x(p + sy) * y(p + sx), SafeLong.two.pow(p + sx + sy)))
      }
  }

  def **(k: Int): Real = pow(k)

  def pow(k: Int): Real = {
    @tailrec
    def loop(b: Real, k: Int, extra: Real): Real =
      if (k == 1)
        b * extra
      else
        loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    this match {
      case Exact(n) =>
        Exact(n.pow(k))
      case _ =>
        if (k < 0) {
          reciprocal.pow(-k)
        } else if (k == 0) {
          Real.one
        } else if (k == 1) {
          this
        } else {
          loop(x, k - 1, x)
        }
    }
  }

  def /(y: Real): Real = x * y.reciprocal

  def tmod(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx.tmod(ny))
    case _ =>
      Real { p =>
        val d = x / y
        val s = d(2)
        val d2 = if (s >= 0) d.floor else d.ceil
        x - d2 * y (p)
      }
  }

  def tquot(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx.tquot(ny))
    case _ =>
      Real { p =>
        val d = x / y
        val s = d(2)
        val d2 = if (s >= 0) d.floor else d.ceil
        d2(p)
      }
  }

  /* TODO: what to do with this definition of gcd/lcm?
  def gcd(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx gcd ny)
    case _ => Real({ p =>
      val g = x.toRational(p) gcd y.toRational(p)
      roundUp(g * SafeLong.two.pow(p))
    })
  }

  def lcm(y: Real): Real = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx lcm ny)
    case _ => Real({ p =>
      val g = x.toRational(p) lcm y.toRational(p)
      roundUp(g * SafeLong.two.pow(p))
    })
  }
   */

  def ceil: Real = x match {
    case Exact(n) => Exact(n.ceil)
    case _ =>
      Real { p =>
        val n = x(p)
        val t = SafeLong.two.pow(p)
        val m = n % t
        if (m == 0) n
        else if (n.signum >= 0) n + t - m
        else n - m
      }
  }

  def floor: Real = x match {
    case Exact(n) => Exact(n.floor)
    case _ =>
      Real { p =>
        val n = x(p)
        val t = SafeLong.two.pow(p)
        val m = n % t
        if (n.signum >= 0) n - m else n - t - m
      }
  }

  def round: Real = x match {
    case Exact(n) => Exact(n.round)
    case _ =>
      Real { p =>
        val n = x(p)
        val t = SafeLong.two.pow(p)
        val h = t / 2
        val m = n % t
        if (m < h) n - m else n - m + t
      }
  }

  def isWhole: Boolean = x match {
    case Exact(n) =>
      n.isWhole
    case _ =>
      val n = x(Real.bits)
      val t = SafeLong.two.pow(Real.bits)
      n % t == 0
  }

  def sqrt: Real = Real(p => x(p * 2).sqrt)
  def nroot(k: Int): Real =
    if (k >= 0) Real(p => x(p * k).nroot(k))
    else Real(p => x.reciprocal.nroot(math.abs(k))(p))

  def fpow(r: Rational): Real =
    Real { p =>
      val r2 = r.limitToInt
      val n = r2.numerator
      val d = r2.denominator
      x.pow(n.toInt).nroot(d.toInt)(p)
    }

  // a bit hand-wavy
  def fpow(y: Real): Real = y match {
    case Exact(n) => x.fpow(n)
    case _ =>
      Real { p =>
        x.fpow(Rational(y(p), SafeLong.two.pow(p)))(p)
      }
  }

  override def toString: String = x match {
    case Exact(n) => n.toString
    case _        => getString(Real.digits)
  }

  def repr: String = x match {
    case Exact(n) => s"Exact(${n.toString})"
    case _        => s"Inexact(${toRational})"
  }

  def getString(d: Int): String = {
    val b = Real.digitsToBits(d)
    val r = Rational(x(b) * SafeLong.ten.pow(d), SafeLong.two.pow(b))
    val m = roundUp(r)
    val (sign, str) = m.signum match {
      case -1 => ("-", m.abs.toString)
      case 0  => ("", "0")
      case 1  => ("", m.toString)
    }
    val i = str.length - d
    val s = if (i > 0) {
      sign + str.substring(0, i) + "." + str.substring(i)
    } else {
      sign + "0." + "0" * -i + str
    }
    s.replaceAll("0+$", "").replaceAll("\\.$", "")
  }
}

object Real extends RealInstances {

  val zero: Real = Exact(Rational.zero)
  val one: Real = Exact(Rational.one)
  val two: Real = Exact(Rational(2))
  val four: Real = Exact(Rational(4))

  def apply(f: Int => SafeLong): Real = Inexact(f)

  implicit def apply(n: Int): Real = Exact(Rational(n))
  implicit def apply(n: Long): Real = Exact(Rational(n))
  implicit def apply(n: BigInt): Real = Exact(Rational(n))
  implicit def apply(n: SafeLong): Real = Exact(Rational(n))
  implicit def apply(n: Rational): Real = Exact(n)
  implicit def apply(n: Double): Real = Exact(Rational(n))
  implicit def apply(n: BigDecimal): Real = Exact(Rational(n))

  def apply(s: String): Real = Exact(Rational(s))

  lazy val pi: Real =
    Real(16) * atan(Real(Rational(1, 5))) - Real.four * atan(Real(Rational(1, 239)))

  lazy val e: Real =
    exp(Real.one)

  lazy val phi: Real =
    (Real.one + Real(5).sqrt) / Real.two

  def log(x: Real): Real = {
    val t = x(2)
    val n = sizeInBase(t, 2) - 3
    if (t < 0) throw new ArithmeticException("log of negative number")
    else if (t < 4) -log(x.reciprocal)
    else if (t < 8) logDr(x)
    else logDr(div2n(x, n)) + Real(n) * log2
  }

  def exp(x: Real): Real = {
    val u = x / log2
    val n = u(0)
    val s = x - Real(n) * log2
    if (!n.isValidInt) throw new ArithmeticException("invalid power in exp")
    else if (n < 0) div2n(expDr(s), -n.toInt)
    else if (n > 0) mul2n(expDr(s), n.toInt)
    else expDr(s)
  }

  def sin(x: Real): Real = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * Real(s)
    val m = (s % 8).toInt
    val n = if (m < 0) m + 8 else m
    n match {
      case 0 => sinDr(y)
      case 1 => sqrt1By2 * (cosDr(y) + sinDr(y))
      case 2 => cosDr(y)
      case 3 => sqrt1By2 * (cosDr(y) - sinDr(y))
      case 4 => -sinDr(y)
      case 5 => -sqrt1By2 * (cosDr(y) + sinDr(y))
      case 6 => -cosDr(y)
      case 7 => -sqrt1By2 * (cosDr(y) - sinDr(y))
    }
  }

  def cos(x: Real): Real = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * Real(s)
    val m = (s % 8).toInt
    val n = if (m < 0) m + 8 else m
    n match {
      case 0 => cosDr(y)
      case 1 => sqrt1By2 * (cosDr(y) - sinDr(y))
      case 2 => -sinDr(y)
      case 3 => -sqrt1By2 * (cosDr(y) + sinDr(y))
      case 4 => -cosDr(y)
      case 5 => -sqrt1By2 * (cosDr(y) - sinDr(y))
      case 6 => sinDr(y)
      case 7 => sqrt1By2 * (cosDr(y) + sinDr(y))
    }
  }

  def tan(x: Real): Real = sin(x) / cos(x)

  def atan(x: Real): Real = {
    val t = x(2)
    val xp1 = x + Real.one
    val xm1 = x - Real.one
    if (t < -5) atanDr(-x.reciprocal) - piBy2
    else if (t == -4) -piBy4 - atanDr(xp1 / xm1)
    else if (t < 4) atanDr(x)
    else if (t == 4) piBy4 + atanDr(xm1 / xp1)
    else piBy2 - atanDr(x.reciprocal)
  }

  def atan2(y: Real, x: Real): Real = Real { p =>
    var pp = p
    var sx = x(pp).signum
    var sy = y(pp).signum
    // val maxp = p * p
    // while (sx == 0 && sy == 0 && pp < maxp) {
    while (sx == 0 && sy == 0) {
      sx = x(pp).signum
      sy = y(pp).signum
      pp += 1
    }
    if (sx > 0) {
      atan(y / x)(p)
    } else if (sy >= 0 && sx < 0) {
      atan(y / x) + Real.pi(p)
    } else if (sy < 0 && sx < 0) {
      atan(y / x) - Real.pi(p)
    } else if (sy > 0) {
      Real.pi / Real.two(p)
    } else if (sy < 0) {
      -Real.pi / Real.two(p)
    } else {
      throw new IllegalArgumentException("atan2(0, 0) is undefined")
      // // ugh
      // Real.zero
      // //sys.error("undefined sx=%s sy=%s" format (sx, sy))
    }
  }

  def asin(x: Real): Real = {
    val x0 = x(0)
    val s = (Real.one - x * x).sqrt
    x0.signum match {
      case n if n > 0 => Real.pi / Real.two - atan(s / x)
      case 0          => atan(x / s)
      case _          => -Real.pi / Real.two - atan(s / x)
    }
  }

  def acos(x: Real): Real = Real.pi / Real.two - asin(x)

  def sinh(x: Real): Real = {
    val y = exp(x)
    (y - y.reciprocal) / Real.two
  }

  def cosh(x: Real): Real = {
    val y = exp(x)
    (y + y.reciprocal) / Real.two
  }

  def tanh(x: Real): Real = {
    val y = exp(x);
    val y2 = y.reciprocal
    (y - y2) / (y + y2)
  }

  def asinh(x: Real): Real = log(x + (x * x + Real.one).sqrt)
  def acosh(x: Real): Real = log(x + (x * x - Real.one).sqrt)
  def atanh(x: Real): Real = log((Real.one + x) / (Real.one - x)) / Real.two

  def digits: Int = 40
  def bits: Int = digitsToBits(digits)

  def digitsToBits(n: Int): Int =
    spire.math.ceil(n * (spire.math.log(10.0) / spire.math.log(2.0))).toInt + 4

  def sizeInBase(n: SafeLong, base: Int): Int = {
    def loop(n: SafeLong, acc: Int): Int = if (n <= 1) acc + 1 else loop(n / base, acc + 1)
    loop(n.abs, 0)
  }

  def roundUp(r: Rational): SafeLong = SafeLong(r.round.toBigInt)

  def div2n(x: Real, n: Int): Real =
    Real(p => if (p >= n) x(p - n) else roundUp(Rational(x(p), SafeLong.two.pow(n))))

  def mul2n(x: Real, n: Int): Real =
    Real(p => x(p + n))

  lazy val piBy2 = div2n(pi, 1)

  lazy val piBy4 = div2n(pi, 2)

  lazy val log2 = div2n(logDrx(Real.two.reciprocal), 1)

  lazy val sqrt1By2 = Real.two.reciprocal.sqrt

  def accumulate(total: SafeLong, xs: LazyList[SafeLong], cs: LazyList[Rational]): SafeLong = {
    ((xs, cs): @unchecked) match {
      case (_, Seq()) => total
      case (Seq(), _) => sys.error("nooooo")
      case (x #:: xs, c #:: cs) =>
        val t = roundUp(c * Rational(x))
        if (t == 0) total else accumulate(total + t, xs, cs)
    }
  }

  @deprecated("prefer LazyList instead", "0.17.0")
  def accumulate(total: SafeLong, xs: Stream[SafeLong], cs: Stream[Rational]): SafeLong = {
    import scala.#::
    ((xs, cs): @unchecked) match {
      case (_, Stream.Empty) => total
      case (Stream.Empty, _) => sys.error("nooooo")
      case (x #:: xs, c #:: cs) =>
        val t = roundUp(c * Rational(x))
        if (t == 0) total else accumulate(total + t, xs, cs)
    }
  }

  private[spire] def powerSeries(ps: LazyList[Rational], terms: Int => Int, x: Real): Real = {
    Real { p =>
      val t = terms(p)
      val l2t = 2 * sizeInBase(SafeLong(t) + 1, 2) + 6
      val p2 = p + l2t
      val xr = x(p2)
      val xn = SafeLong.two.pow(p2)
      if (xn == 0) sys.error("oh no")
      def g(yn: SafeLong): SafeLong = roundUp(Rational(yn * xr, xn))
      val num = accumulate(SafeLong.zero, LazyList.iterate(xn)(g), ps.take(t))
      val denom = SafeLong.two.pow(l2t)
      roundUp(Rational(num, denom))
    }
  }

  private[spire] def accSeq(f: (Rational, SafeLong) => Rational): LazyList[Rational] = {
    def loop(r: Rational, n: SafeLong): LazyList[Rational] =
      r #:: loop(f(r, n), n + 1)
    loop(Rational.one, SafeLong.one)
  }

  def expDr(x: Real): Real =
    powerSeries(accSeq((r, n) => r / n), n => n, x)

  def logDr(x: Real): Real = {
    val y = (x - Real.one) / x
    y * logDrx(y)
  }

  def logDrx(x: Real): Real = {
    powerSeries(LazyList.from(1).map(n => Rational(1, n)), _ + 1, x)
  }

  def sinDr(x: Real): Real =
    x * powerSeries(accSeq((r, n) => -r * Rational(1, 2 * n * (2 * n + 1))), n => n, x * x)

  def cosDr(x: Real): Real =
    powerSeries(accSeq((r, n) => -r * Rational(1, 2 * n * (2 * n - 1))), n => n, x * x)

  def atanDr(x: Real): Real = {
    val y = x * x + Real(1)
    x / y * atanDrx(x * x / y)
  }

  def atanDrx(x: Real): Real =
    // powerSeries(accSeq((r, n) => r * (Rational(2*n, 2*n + 1))), _ + 1, x)
    powerSeries(accSeq((r, n) => r * Rational(2 * n, 2 * n + 1)), _ * 2, x)

  case class Exact(n: Rational) extends Real {
    def apply(p: Int): SafeLong = Real.roundUp(Rational(2).pow(p) * n)
  }

  case class Inexact(f: Int => SafeLong) extends Real {
    @volatile private[spire] var memo: Option[(Int, SafeLong)] = None

    def apply(p: Int): SafeLong = memo match {
      case Some(bits, value) if bits >= p =>
        Real.roundUp(Rational(value, SafeLong(2).pow(bits - p)))
      case _ =>
        val result = f(p)
        memo = Some((p, result))
        result
    }
  }
}

trait RealInstances {
  implicit final val algebra
    : Fractional[Real] with TruncatedDivisionCRing[Real] with Trig[Real] with Field[Real] with Order[Real] =
    new RealAlgebra
  import NumberTag._
  implicit final val RealTag: NumberTag[Real] = new LargeTag[Real](Exact, Real.zero)
}

@SerialVersionUID(0L)
class RealAlgebra extends RealIsFractional

trait RealIsFractional
    extends Fractional[Real]
    with TruncatedDivisionCRing[Real]
    with Trig[Real]
    with Field[Real]
    with Order[Real] {
  def order = this

  override def abs(x: Real): Real = x.abs
  override def signum(x: Real): Int = x.signum

  override def eqv(x: Real, y: Real): Boolean = x === y
  def compare(x: Real, y: Real): Int = x.compare(y)

  def zero: Real = Real.zero
  def one: Real = Real.one
  def negate(x: Real): Real = -x
  def plus(x: Real, y: Real): Real = x + y
  override def minus(x: Real, y: Real): Real = x - y
  def times(x: Real, y: Real): Real = x * y

  def toBigIntOpt(x: Real): Opt[BigInt] = if (x.isWhole) Opt(x.toRational.toBigInt) else Opt.empty[BigInt]
  def tquot(x: Real, y: Real): Real = x.tquot(y)
  def tmod(x: Real, y: Real): Real = x.tmod(y)

  override def reciprocal(x: Real): Real = x.reciprocal
  def div(x: Real, y: Real): Real = x / y

  override def sqrt(x: Real): Real = x.sqrt
  def nroot(x: Real, k: Int): Real = x.nroot(k)
  def fpow(x: Real, y: Real): Real = x.fpow(y)

  def acos(a: Real): Real = Real.acos(a)
  def asin(a: Real): Real = Real.asin(a)
  def atan(a: Real): Real = Real.atan(a)
  def atan2(y: Real, x: Real): Real = Real.atan2(y, x)
  def cos(a: Real): Real = Real.cos(a)
  def cosh(x: Real): Real = Real.cosh(x)
  def e: Real = Real.e
  def exp(x: Real): Real = Real.exp(x)
  def expm1(x: Real): Real = Real.exp(x) - Real.one
  def log(x: Real): Real = Real.log(x)
  def log1p(x: Real): Real = Real.log(Real.one + x)
  def pi: Real = Real.pi
  def sin(x: Real): Real = Real.sin(x)
  def sinh(x: Real): Real = Real.sinh(x)
  def tan(x: Real): Real = Real.tan(x)
  def tanh(x: Real): Real = Real.tanh(x)
  def toDegrees(a: Real): Real = a / (Real.two * Real.pi) * Real(360)
  def toRadians(a: Real): Real = a / Real(360) * (Real.two * Real.pi)

  def ceil(x: Real): Real = x.ceil
  def floor(x: Real): Real = x.floor
  def isWhole(x: Real): Boolean = x.isWhole
  def round(x: Real): Real = x.round

  def toByte(x: Real): Byte = x.toRational.toByte
  def toInt(x: Real): Int = x.toRational.toInt
  def toShort(x: Real): Short = x.toRational.toShort
  def toLong(x: Real): Long = x.toRational.toLong
  def toFloat(x: Real): Float = x.toRational.toFloat
  def toDouble(x: Real): Double = x.toRational.toDouble
  def toBigInt(x: Real): BigInt = x.toRational.toBigInt
  def toBigDecimal(x: Real): BigDecimal = x.toRational.toBigDecimal(java.math.MathContext.DECIMAL64)
  def toRational(x: Real): Rational = x.toRational
  def toAlgebraic(x: Real): Algebraic = Algebraic(x.toRational) // FIXME
  def toReal(x: Real): Real = x
  def toNumber(x: Real): Number = Number(x.toRational)
  def toString(x: Real): String = x.toString

  def toType[B](x: Real)(implicit ev: ConvertableTo[B]): B =
    ev.fromReal(x)

  def fromByte(n: Byte): Real = Real(n)
  def fromShort(n: Short): Real = Real(n)
  def fromFloat(n: Float): Real = Real(n)
  def fromLong(n: Long): Real = Real(n)
  override def fromBigInt(n: BigInt): Real = Real(n)
  def fromBigDecimal(n: BigDecimal): Real = Real(n)
  def fromRational(n: Rational): Real = Real(n)
  def fromAlgebraic(n: Algebraic): Real = n.evaluateWith[Real]
  def fromReal(n: Real): Real = n

  def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): Real =
    ev.toReal(b)
}
