package spire.math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import spire.syntax.nroot._

sealed trait CReal extends ScalaNumber with ScalaNumericConversions { x =>

  import CReal.{roundUp, Exact, Inexact}

  def apply(p: Int): SafeLong

  def toRational(p: Int): Rational = this match {
    case Exact(n) => n
    case _ => Rational(x(p), SafeLong.two.pow(p))
  }

  def toRational: Rational = toRational(CReal.bits)

  // ugh scala.math
  def doubleValue(): Double = toRational.toDouble
  def floatValue(): Float = toRational.toFloat
  def intValue(): Int = toRational.toInt
  def longValue(): Long = toRational.toLong
  def underlying(): Object = this

  override def hashCode(): Int = toRational.hashCode

  override def equals(y: Any): Boolean = y match {
    case y: CReal => x eqv y
    case y => toRational.equals(y)
  }

  def eqv(y: CReal): Boolean = (x, y) match {
    case (Exact(nx), Exact(ny)) => nx == ny
    case _ => (x - y).signum == 0 || x.toString == y.toString
  }

  def compare(y: CReal): Int = (x, y) match {
    case (Exact(nx), Exact(ny)) => nx compare ny
    case _ => (x - y)(CReal.bits).signum
  }

  def min(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx min ny)
    case _ => CReal(p => x(p) min y(p))
  }

  def max(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx max ny)
    case _ => CReal(p => x(p) max y(p))
  }

  def abs(): CReal = this match {
    case Exact(n) => Exact(n.abs)
    case _ => CReal(p => x(p).abs)
  }

  def signum(): Int = this match {
    case Exact(n) => n.signum
    case _ => x(CReal.bits).signum
  }

  def unary_-(): CReal = this match {
    case Exact(n) => Exact(-n)
    case _ => CReal(p => -x(p))
  }

  def reciprocal(): CReal = {
    def findNonzero(i: Int): Int =
      if (SafeLong.three <= x(i).abs) i else findNonzero(i + 1)

    this match {
      case Exact(n) => Exact(n.reciprocal)
      case _ => CReal({p =>
        val s = findNonzero(0)
        roundUp(Rational(SafeLong.two.pow(2 * p + 2 * s + 2), x(p + 2 * s + 2)))
      })
    }
  }

  def +(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx + ny)
    case (Exact(Rational.zero), _) => y
    case (_, Exact(Rational.zero)) => x
    case _ => CReal(p => roundUp(Rational(x(p + 2) + y(p + 2), 4)))
  }

  def -(y: CReal): CReal = x + (-y)

  def *(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx * ny)
    case (Exact(Rational.zero), _) => CReal.zero
    case (_, Exact(Rational.zero)) => CReal.zero
    case (Exact(Rational.one), _) => y
    case (_, Exact(Rational.one)) => x
    case _ => CReal({p =>
      val x0 = x(0).abs + 2
      val y0 = y(0).abs + 2
      val sx = CReal.sizeInBase(x0, 2) + 3
      val sy = CReal.sizeInBase(y0, 2) + 3
      roundUp(Rational(x(p + sy) * y(p + sx), SafeLong.two.pow(p + sx + sy)))
    })
  }

  def pow(k: Int): CReal = {
    def loop(b: CReal, k: Int, extra: CReal): CReal =
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
          CReal.one
        } else if (k == 1) {
          this
        } else {
          loop(x, k - 1, x)
        }
    }
  }

  def /(y: CReal): CReal = x * y.reciprocal

  def %(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx % ny)
    case _ => CReal({ p => 
      val d = x / y
      val s = d(2)
      val d2 = if (s >= 0) d.floor else d.ceil
        (x - d2 * y)(p)
    })
  }

  def /~(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx /~ ny)
    case _ => CReal({ p =>
      val d = x / y
      val s = d(2)
      val d2 = if (s >= 0) d.floor else d.ceil
      d2(p)
    })
  }

  def gcd(y: CReal): CReal = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx gcd ny)
    case _ => CReal({ p =>
      val g = x.toRational(p) gcd y.toRational(p)
      roundUp(g * SafeLong.two.pow(p))
    })
  }

  def ceil(): CReal = x match {
    case Exact(n) => Exact(n.ceil)
    case _ => CReal({ p =>
      val n = x(p)
      val t = SafeLong.two.pow(p)
      val m = n % t
      if (m == 0) n else n + t - m
    })
  }

  def floor(): CReal = x match {
    case Exact(n) => Exact(n.floor)
    case _ => CReal({ p =>
      val n = x(p)
      val t = SafeLong.two.pow(p)
      val m = n % t
      n - m
    })
  }

  def round(): CReal = x match {
    case Exact(n) => Exact(n.round)
    case _ => CReal({ p =>
      val n = x(p)
      val t = SafeLong.two.pow(p)
      val h = t / 2
      val m = n % t
      if (m < h) n - m else n - m + t
    })
  }

  def isWhole(): Boolean = x match {
    case Exact(n) =>
      n.isWhole
    case _ =>
      val n = x(CReal.bits)
      val t = SafeLong.two.pow(CReal.bits)
        (n % t) == 0
  }

  def sqrt(): CReal = CReal(p => x(p * 2).sqrt)
  def nroot(k: Int): CReal = CReal(p => x(p * k).nroot(k))

  def fpow(r: Rational): CReal =
    CReal({ p =>
      val r2 = r.limitToInt
      val n = r2.numerator
      val d = r2.denominator
      x.pow(n.toInt).nroot(d.toInt)(p)
    })

  // a bit hand-wavy
  def fpow(y: CReal): CReal = y match {
    case Exact(n) => x.fpow(n)
    case _ => CReal({ p =>
      x.fpow(Rational(y(p), SafeLong.two.pow(p)))(p)
    })
  }

  override def toString: String = x match {
    case Exact(n) => n.toString
    case _ => getString(CReal.digits)
  }

  def repr: String = x match {
    case Exact(n) => s"Exact(${n.toString})"
    case _ => s"Inexact(${toRational})"
  }

  def getString(d: Int): String = {
    val b = CReal.digitsToBits(d)
    val r = Rational(x(b) * SafeLong.ten.pow(d), SafeLong.two.pow(b))
    val m = roundUp(r)
    val (sign, str) = m.signum match {
      case -1 => ("-", m.abs.toString)
      case 0 => ("", "0")
      case 1 => ("", m.toString)
    }
    val i = str.length - d
    val s = if (i > 0) {
      sign + str.substring(0, i) + "." + str.substring(i)
    } else {
      sign + "0." + ("0" * -i) + str
    }
    s.replaceAll("0+$", "").replaceAll("\\.$", "")
  }
}

object CReal {
  import spire.algebra._

  val zero: CReal = Exact(Rational.zero)
  val one: CReal = Exact(Rational.one)
  val two: CReal = Exact(Rational(2))
  val four: CReal = Exact(Rational(4))

  def apply(f: Int => SafeLong): CReal = Inexact(f)
  def apply(n: Long): CReal = Exact(Rational(n))
  def apply(n: BigInt): CReal = Exact(Rational(n))
  def apply(n: SafeLong): CReal = Exact(Rational(n))
  def apply(n: Rational): CReal = Exact(n)
  def apply(n: Double): CReal = Exact(Rational(n))
  def apply(n: BigDecimal): CReal = Exact(Rational(n))
  def apply(s: String): CReal = Exact(Rational(s))

  lazy val pi: CReal = CReal(16) * atan(CReal(Rational(1, 5))) - CReal.four * atan(CReal(Rational(1, 239)))

  lazy val e: CReal = exp(CReal.one)

  def log(x: CReal): CReal = {
    val t = x(2)
    val n = sizeInBase(t, 2) - 3
    if (t < 0) sys.error("log of negative number")
    else if (t < 4) -log(x.reciprocal)
    else if (t < 8) logDr(x)
    else logDr(div2n(x, n)) + CReal(n) * log2
  }

  def exp(x: CReal): CReal = {
    val u = x / log2
    val n = u(0)
    val s = x - CReal(n) * log2
    if (!n.isValidInt) sys.error("sorry")
    else if (n < 0) div2n(expDr(s), -n.toInt)
    else if (n > 0) mul2n(expDr(s), n.toInt)
    else expDr(s)
  }

  def sin(x: CReal): CReal = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * CReal(s)
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

  def cos(x: CReal): CReal = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * CReal(s)
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

  def tan(x: CReal): CReal = sin(x) / cos(x)

  def atan(x: CReal): CReal = {
    val t = x(2)
    val xp1 = x + CReal.one
    val xm1 = x - CReal.one
    if (t < -5) atanDr(-x.reciprocal) - piBy2
    else if (t == -4) -piBy4 - atanDr(xp1 / xm1)
    else if (t < 4) atanDr(x)
    else if (t == 4) piBy4 + atanDr(xm1 / xp1)
    else piBy2 - atanDr(x.reciprocal)
  }

  def atan2(y: CReal, x: CReal): CReal = CReal({ p =>
    val sx = x(p).signum
    val sy = y(p).signum
    if (sx > 0) {
      atan(y / x)(p)
    } else if (sy >= 0 && sx < 0) {
      (atan(y / x) + CReal.pi)(p)
    } else if (sy < 0 && sx < 0) {
      (atan(y / x) - CReal.pi)(p)
    } else if (sy > 0) {
      (CReal.pi / CReal.two)(p)
    } else if (sy < 0) {
      (-CReal.pi / CReal.two)(p)
    } else {
      sys.error("undefined")
    }
  })

  def asin(x: CReal): CReal = {
    val x0 = x(0)
    val s = (CReal.one - x * x).sqrt
    x0.signum match {
      case n if n > 0 => (CReal.pi / CReal.two) - atan(s / x)
      case 0 => atan(x / s)
      case _ => (-CReal.pi / CReal.two) - atan(s / x)
    }
  }

  def acos(x: CReal): CReal  = (CReal.pi / CReal.two) - asin(x)

  def sinh(x: CReal): CReal = {
    val y = exp(x)
    (y - y.reciprocal) / CReal.two
  }

  def cosh(x: CReal): CReal = {
    val y = exp(x)
    (y + y.reciprocal) / CReal.two
  }

  def tanh(x: CReal): CReal = {
    val y = exp(x);
    val y2 = y.reciprocal
    (y - y2) / (y + y2)
  }

  def asinh(x: CReal): CReal = log(x + (x * x + CReal.one).sqrt)
  def acosh(x: CReal): CReal = log(x + (x * x - CReal.one).sqrt)
  def atanh(x: CReal): CReal = log((CReal.one + x) / (CReal.one - x)) / CReal.two

  def digits: Int = 40
  def bits: Int = digitsToBits(digits)

  def digitsToBits(n: Int): Int =
    spire.math.ceil(n * (spire.math.log(10.0) / spire.math.log(2.0))).toInt + 4

  def sizeInBase(n: SafeLong, base: Int): Int = {
    def loop(n: SafeLong, acc: Int): Int = if (n <= 1) acc + 1 else loop(n / base, acc + 1)
    loop(n.abs, 0)
  }

  def roundUp(r: Rational): SafeLong = SafeLong(r.round.toBigInt)

  def div2n(x: CReal, n: Int): CReal =
    CReal(p => if (p >= n) x(p - n) else roundUp(Rational(x(p), SafeLong.two.pow(n))))

  def mul2n(x: CReal, n: Int): CReal =
    CReal(p => x(p + n))

  lazy val piBy2 = div2n(pi, 1)

  lazy val piBy4 = div2n(pi, 2)

  lazy val log2 = div2n(logDrx(CReal.two.reciprocal), 1)

  lazy val sqrt1By2 = CReal.two.reciprocal.sqrt

  def accumulate(total: SafeLong, xs: Stream[SafeLong], cs: Stream[Rational]): SafeLong = {
    (xs, cs) match {
      case (_, Stream.Empty) => total
      case (Stream.Empty, _) => sys.error("nooooo")
      case (x #:: xs, c #:: cs) =>
        val t = roundUp(c * Rational(x))
        if (t == 0) total else accumulate(total + t, xs, cs)
    }
  }

  private[spire] def powerSeries(ps: Stream[Rational], terms: Int => Int, x: CReal): CReal = {
    CReal({p =>
      val t = terms(p)
      val l2t = 2 * sizeInBase(SafeLong(t) + 1, 2) + 6
      val p2 = p + l2t
      val xr = x(p2)
      val xn = SafeLong.two.pow(p2)
      if (xn == 0) sys.error("oh no")
      def g(yn: SafeLong): SafeLong = roundUp(Rational(yn * xr, xn))
      val num = accumulate(SafeLong.zero, Stream.iterate(xn)(g), ps.take(t))
      val denom = SafeLong.two.pow(l2t)
      roundUp(Rational(num, denom))
    })
  }

  private[spire] def accSeq(f: (Rational, SafeLong) => Rational): Stream[Rational] = {
    def loop(r: Rational, n: SafeLong): Stream[Rational] =
      r #:: loop(f(r, n), n + 1)
    loop(Rational.one, SafeLong.one)
  }

  def expDr(x: CReal): CReal =
    powerSeries(accSeq((r, n) => r / n), n => n, x)

  def logDr(x: CReal): CReal = {
    val y = (x - CReal.one) / x
    y * logDrx(y)
  }

  def logDrx(x: CReal): CReal = {
    powerSeries(Stream.from(1).map(n => Rational(1, n)), _ + 1, x)
  }

  def sinDr(x: CReal): CReal =
    x * powerSeries(accSeq((r, n) => -r * Rational(1, 2*n*(2*n+1))), n => n, x * x)

  def cosDr(x: CReal): CReal =
    powerSeries(accSeq((r, n) => -r * Rational(1, 2*n*(2*n-1))), n => n, x * x)

  def atanDr(x: CReal): CReal = {
    val y = x * x + CReal(1)
    (x / y) * atanDrx((x * x) / y)
  }

  def atanDrx(x: CReal): CReal =
    powerSeries(accSeq((r, n) => r * (Rational(2*n, 2*n + 1))), _ + 1, x)

  implicit val algebra = new Fractional[CReal] with Order[CReal] with Signed[CReal] with Trig[CReal] {
    def abs(x: CReal): CReal = x.abs
    def signum(x: CReal): Int = x.signum

    override def eqv(x: CReal, y: CReal): Boolean = x eqv y
    def compare(x: CReal, y: CReal): Int = x compare y

    def zero: CReal = CReal.zero
    def one: CReal = CReal.one
    def negate(x: CReal): CReal = -x
    def plus(x: CReal, y: CReal): CReal = x + y
    override def minus(x: CReal, y: CReal): CReal = x - y
    def times(x: CReal, y: CReal): CReal = x * y

    def gcd(x: CReal, y: CReal): CReal = x gcd y
    def quot(x: CReal, y: CReal): CReal = x /~ y
    def mod(x: CReal, y: CReal): CReal = x % y

    override def reciprocal(x: CReal): CReal = x.reciprocal
    def div(x: CReal, y: CReal): CReal = x / y

    override def sqrt(x: CReal): CReal = x.sqrt
    def nroot(x: CReal, k: Int): CReal = x.nroot(k)
    def fpow(x: CReal, y: CReal): CReal = x fpow y

    def acos(a: CReal): CReal = CReal.acos(a)
    def asin(a: CReal): CReal = CReal.asin(a)
    def atan(a: CReal): CReal = CReal.atan(a)
    def atan2(y: CReal, x: CReal): CReal = CReal.atan2(y, x)
    def cos(a: CReal): CReal = CReal.cos(a)
    def cosh(x: CReal): CReal = CReal.cosh(x)
    def e: CReal = CReal.e
    def exp(x: CReal): CReal = CReal.exp(x)
    def expm1(x: CReal): CReal = CReal.exp(CReal.one) - CReal.one
    def log(x: CReal): CReal = CReal.log(x)
    def log1p(x: CReal): CReal = CReal.log(CReal.one + x)
    def pi: CReal = CReal.pi
    def sin(x: CReal): CReal = CReal.sin(x)
    def sinh(x: CReal): CReal = CReal.sinh(x)
    def tan(x: CReal): CReal = CReal.tan(x)
    def tanh(x: CReal): CReal = CReal.tanh(x)
    def toDegrees(a: CReal): CReal = a / (CReal.two * CReal.pi) * CReal(360)
    def toRadians(a: CReal): CReal = a / CReal(360) * (CReal.two * CReal.pi)

    def ceil(x: CReal): CReal = x.ceil
    def floor(x: CReal): CReal = x.floor
    def isWhole(x: CReal): Boolean = x.isWhole
    def round(x: CReal): CReal = x.round

    def toRational(x: CReal): Rational = x.toRational
    def toDouble(x: CReal): Double = x.toRational.toDouble
    def toBigDecimal(x: CReal): BigDecimal = x.toRational.toBigDecimal
    def toBigInt(x: CReal): BigInt = x.toRational.toBigInt
    def toByte(x: CReal): Byte = x.toRational.toByte
    def toFloat(x: CReal): Float = x.toRational.toFloat
    def toInt(x: CReal): Int = x.toRational.toInt
    def toLong(x: CReal): Long = x.toRational.toLong
    def toNumber(x: CReal): Number = Number(x.toRational)
    def toShort(x: CReal): Short = x.toRational.toShort
    def toString(x: CReal): String = x.toString

    def toType[B](x: CReal)(implicit ev: ConvertableTo[B]): B =
      ev.fromRational(x.toRational)

    def fromBigDecimal(n: BigDecimal): CReal = CReal(n)
    def fromBigInt(n: BigInt): CReal = CReal(n)
    def fromByte(n: Byte): CReal = CReal(n)
    def fromFloat(n: Float): CReal = CReal(n)
    def fromLong(n: Long): CReal = CReal(n)
    def fromRational(n: Rational): CReal = CReal(n)
    def fromShort(n: Short): CReal = CReal(n)

    def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): CReal =
      CReal(ev.toRational(b))
  }

  case class Exact(n: Rational) extends CReal { x =>
    def apply(p: Int): SafeLong = CReal.roundUp(Rational(2).pow(p) * n)
  }

  case class Inexact(f: Int => SafeLong) extends CReal {
    @volatile private[this] var memo: Option[(Int, SafeLong)] = None

    def apply(p: Int): SafeLong = memo match {
      case Some((bits, value)) if bits >= p =>
        value >> (bits - p)
      case _ =>
        val result = f(p)
        memo = Some((p, result))
        result
    }
  }
}
