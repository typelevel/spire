package spire.math

import spire.syntax.nroot._

sealed trait CR { x =>

  def apply(p: Int): SafeLong

  import CR.roundUp

  def toRational(p: Int): Rational = this match {
    case Exact(n) => n
    case _ => Rational(x(p), SafeLong.two.pow(p))
  }

  def toRational: Rational = toRational(CR.bits)

  override def equals(y: Any): Boolean = y match {
    case y: CR => x eqv y
    case y => toRational.equals(y)
  }

  def eqv(y: CR): Boolean = (x, y) match {
    case (Exact(nx), Exact(ny)) => nx == ny
    case _ => (x - y)(CR.bits).signum == 0
  }

  def compare(y: CR): Int = (x, y) match {
    case (Exact(nx), Exact(ny)) => nx compare ny
    case _ => (x - y)(CR.bits).signum
  }

  def min(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx min ny)
    case _ => CR(p => x(p) min y(p))
  }

  def max(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx max ny)
    case _ => CR(p => x(p) max y(p))
  }

  def abs(): CR = this match {
    case Exact(n) => Exact(n.abs)
    case _ => CR(p => x(p).abs)
  }

  def signum(): Int = this match {
    case Exact(n) => n.signum
    case _ => x(CR.bits).signum
  }

  def unary_-(): CR = this match {
    case Exact(n) => Exact(-n)
    case _ => CR(p => -x(p))
  }

  def reciprocal(): CR = {
    def findNonzero(i: Int): Int =
      if (SafeLong.three <= x(i).abs) i else findNonzero(i + 1)

    this match {
      case Exact(n) => Exact(n.reciprocal)
      case _ => CR({p =>
        val s = findNonzero(0)
        roundUp(Rational(SafeLong.two.pow(2 * p + 2 * s + 2), x(p + 2 * s + 2)))
      })
    }
  }

  def +(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx + ny)
    case (Exact(Rational.zero), _) => y
    case (_, Exact(Rational.zero)) => x
    case _ => CR(p => roundUp(Rational(x(p + 2) + y(p + 2), 4)))
  }

  def -(y: CR): CR = x + (-y)

  def *(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx * ny)
    case (Exact(Rational.zero), _) => CR.zero
    case (_, Exact(Rational.zero)) => CR.zero
    case (Exact(Rational.one), _) => y
    case (_, Exact(Rational.one)) => x
    case _ => CR({p =>
      val x0 = x(0).abs + 2
      val y0 = y(0).abs + 2
      val sx = CR.sizeInBase(x0, 2) + 3
      val sy = CR.sizeInBase(y0, 2) + 3
      roundUp(Rational(x(p + sy) * y(p + sx), SafeLong.two.pow(p + sx + sy)))
    })
  }

  def pow(k: Int): CR = {
    def loop(b: CR, k: Int, extra: CR): CR =
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
          CR.one
        } else if (k == 1) {
          this
        } else {
          loop(x, k - 1, x)
        }
    }
  }

  def /(y: CR): CR = x * y.reciprocal

  def %(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx % ny)
    case _ => CR({ p => 
      val d = x / y
      val s = d(2)
      val d2 = if (s >= 0) d.floor else d.ceil
        (x - d2 * y)(p)
    })
  }

  def /~(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx /~ ny)
    case _ => CR({ p =>
      val d = x / y
      val s = d(2)
      val d2 = if (s >= 0) d.floor else d.ceil
      d2(p)
    })
  }

  def gcd(y: CR): CR = (x, y) match {
    case (Exact(nx), Exact(ny)) => Exact(nx gcd ny)
    case _ => CR({ p =>
      val g = x.toRational(p) gcd y.toRational(p)
      roundUp(g * SafeLong.two.pow(p))
    })
  }

  def ceil(): CR = x match {
    case Exact(n) => Exact(n.ceil)
    case _ => CR({ p =>
      val n = x(p)
      val t = SafeLong.two.pow(p)
      val m = n % t
      if (m == 0) n else n + t - m
    })
  }

  def floor(): CR = x match {
    case Exact(n) => Exact(n.floor)
    case _ => CR({ p =>
      val n = x(p)
      val t = SafeLong.two.pow(p)
      val m = n % t
      n - m
    })
  }

  def round: CR = x match {
    case Exact(n) => Exact(n.round)
    case _ => CR({ p =>
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
      val n = x(CR.bits)
      val t = SafeLong.two.pow(CR.bits)
        (n % t) == 0
  }

  def sqrt(): CR = CR(p => x(p * 2).sqrt)
  def nroot(k: Int): CR = CR(p => x(p * k).nroot(k))

  def fpow(r: Rational): CR =
    CR({ p =>
      val r2 = r.limitToInt
      val n = r2.numerator
      val d = r2.denominator
      x.pow(n.toInt).nroot(d.toInt)(p)
    })

  // a bit hand-wavy
  def fpow(y: CR): CR = y match {
    case Exact(n) => x.fpow(n)
    case _ => CR({ p =>
      x.fpow(Rational(y(p), SafeLong.two.pow(p)))(p)
    })
  }

  override def toString: String = x match {
    case Exact(n) => n.toString
    case _ => getString(CR.digits)
  }

  def getString(d: Int): String = {
    val b = CR.digitsToBits(d)
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

object CR {
  import spire.algebra._

  val zero = Exact(Rational.zero)
  val one = Exact(Rational.one)
  val two = Exact(Rational(2))
  val four = Exact(Rational(1, 2))

  def apply(f: Int => SafeLong): CR = Inexact(f)
  def apply(n: Long): CR = Exact(Rational(n))
  def apply(n: BigInt): CR = Exact(Rational(n))
  def apply(n: SafeLong): CR = Exact(Rational(n))
  def apply(n: Rational): CR = Exact(n)
  def apply(n: Double): CR = Exact(Rational(n))
  def apply(n: BigDecimal): CR = Exact(Rational(n))
  def apply(s: String): CR = Exact(Rational(s))

  lazy val pi = CR(16) * atan(CR(Rational(1, 5))) - CR.four * atan(CR(Rational(1, 239)))

  lazy val e = exp(CR.one)

  def log(x: CR): CR = {
    val t = x(2)
    val n = sizeInBase(t, 2) - 3
    if (t < 0) sys.error("log of negative number")
    else if (t < 4) -log(x.reciprocal)
    else if (t < 8) logDr(x)
    else logDr(div2n(x, n)) + CR(n) * log2
  }

  def exp(x: CR): CR = {
    val u = x / log2
    val n = u(0)
    val s = x - CR(n) * log2
    if (!n.isValidInt) sys.error("sorry")
    else if (n < 0) div2n(expDr(s), -n.toInt)
    else if (n > 0) mul2n(expDr(s), n.toInt)
    else expDr(s)
  }

  def sin(x: CR): CR = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * CR(s)
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

  def cos(x: CR): CR = {
    val z = x / piBy4
    val s = roundUp(Rational(z(2), 4))
    val y = x - piBy4 * CR(s)
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

  def tan(x: CR): CR = sin(x) / cos(x)

  def atan(x: CR): CR = {
    val t = x(2)
    val xp1 = x + CR.one
    val xm1 = x - CR.one
    if (t < -5) atanDr(-x.reciprocal - piBy2)
    else if (t == -4) -piBy4 - atanDr(xp1 / xm1)
    else if (t < 4) atanDr(x)
    else if (t == 4) piBy4 + atanDr(xm1 / xp1)
    else piBy2 - atanDr(x.reciprocal)
  }

  def atan2(y: CR, x: CR): CR = CR({ p =>
    val sx = x(p).signum
    val sy = y(p).signum
    if (sx > 0) {
      atan(y / x)(p)
    } else if (sy >= 0 && sx < 0) {
      (atan(y / x) + CR.pi)(p)
    } else if (sy < 0 && sx < 0) {
      (atan(y / x) - CR.pi)(p)
    } else if (sy > 0) {
      (CR.pi / CR.two)(p)
    } else if (sy < 0) {
      (-CR.pi / CR.two)(p)
    } else {
      sys.error("undefined")
    }
  })

  def asin(x: CR): CR = {
    val x0 = x(0)
    val s = (CR.one - x * x).sqrt
    if (x0 > 0) pi / CR.two - atan(s / x)
    else if (x0 == 0) atan(x / s)
    else atan(s / x) - pi / CR.two
  }

  def acos(x: CR): CR  = pi / CR.two - asin(x)

  def sinh(x: CR): CR = {
    val y = exp(x)
    (y - y.reciprocal) / CR.two
  }

  def cosh(x: CR): CR = {
    val y = exp(x)
    (y + y.reciprocal) / CR.two
  }

  def tanh(x: CR): CR = {
    val y = exp(x);
    val y2 = y.reciprocal
    (y - y2) / (y + y2)
  }

  def asinh(x: CR): CR = log(x + (x * x + CR.one).sqrt)
  def acosh(x: CR): CR = log(x + (x * x - CR.one).sqrt)
  def atanh(x: CR): CR = log((CR.one + x) / (CR.one - x)) / CR.two

  def digits: Int = 40
  def bits: Int = digitsToBits(digits)

  def digitsToBits(n: Int): Int =
    spire.math.ceil(n * (spire.math.log(10.0) / spire.math.log(2.0))).toInt + 4

  def sizeInBase(n: SafeLong, base: Int): Int = {
    def loop(n: SafeLong, acc: Int): Int = if (n <= 1) acc + 1 else loop(n / base, acc + 1)
    loop(n.abs, 0)
  }

  def roundUp(r: Rational): SafeLong = SafeLong(r.round.toBigInt)

  def div2n(x: CR, n: Int): CR =
    CR(p => if (p >= n) x(p - n) else roundUp(Rational(x(p), SafeLong.two.pow(n))))

  def mul2n(x: CR, n: Int): CR =
    CR(p => x(p + n))

  lazy val piBy2 = div2n(pi, 1)

  lazy val piBy4 = div2n(pi, 2)

  lazy val log2 = div2n(logDrx(CR.two.reciprocal), 1)

  lazy val sqrt1By2 = CR.two.reciprocal.sqrt

  def accumulate(total: SafeLong, xs: Stream[SafeLong], cs: Stream[Rational]): SafeLong = {
    (xs, cs) match {
      case (_, Stream.Empty) => total
      case (Stream.Empty, _) => sys.error("nooooo")
      case (x #:: xs, c #:: cs) =>
        val t = roundUp(c * Rational(x))
        if (t == 0) total else accumulate(total + t, xs, cs)
    }
  }

  private[spire] def powerSeries(ps: Stream[Rational], terms: Int => Int, x: CR): CR = {
    CR({p =>
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

  def expDr(x: CR): CR =
    powerSeries(accSeq((r, n) => r / n), n => n, x)

  def logDr(x: CR): CR = {
    val y = (x - CR.one) / x
    y * logDrx(y)
  }

  def logDrx(x: CR): CR = {
    powerSeries(Stream.from(1).map(n => Rational(1, n)), _ + 1, x)
  }

  def sinDr(x: CR): CR =
    x * powerSeries(accSeq((r, n) => -r * Rational(1, 2*n*(2*n+1))), n => n, x * x)

  def cosDr(x: CR): CR =
    powerSeries(accSeq((r, n) => -r * Rational(1, 2*n*(2*n-1))), n => n, x * x)

  def atanDr(x: CR): CR = {
    val y = x * x + CR(1)
    (x / y) * atanDrx((x * x) / y)
  }

  def atanDrx(x: CR): CR =
    powerSeries(accSeq((r, n) => r * (Rational(2*n, 2*n + 1))), _ + 1, x)

  implicit val algebra = new Fractional[CR] with Order[CR] with Signed[CR] with Trig[CR] {
    def abs(x: CR): CR = x.abs
    def signum(x: CR): Int = x.signum

    override def eqv(x: CR, y: CR): Boolean = x eqv y
    def compare(x: CR, y: CR): Int = x compare y

    def zero: CR = CR.zero
    def one: CR = CR.one
    def negate(x: CR): CR = -x
    def plus(x: CR, y: CR): CR = x + y
    override def minus(x: CR, y: CR): CR = x - y
    def times(x: CR, y: CR): CR = x * y

    def gcd(x: CR, y: CR): CR = x gcd y
    def quot(x: CR, y: CR): CR = x /~ y
    def mod(x: CR, y: CR): CR = x % y

    override def reciprocal(x: CR): CR = x.reciprocal
    def div(x: CR, y: CR): CR = x / y

    override def sqrt(x: CR): CR = x.sqrt
    def nroot(x: CR, k: Int): CR = x.nroot(k)
    def fpow(x: CR, y: CR): CR = x fpow y

    def acos(a: CR): CR = CR.acos(a)
    def asin(a: CR): CR = CR.asin(a)
    def atan(a: CR): CR = CR.atan(a)
    def atan2(y: CR, x: CR): CR = CR.atan2(y, x)
    def cos(a: CR): CR = CR.cos(a)
    def cosh(x: CR): CR = CR.cosh(x)
    def e: CR = CR.e
    def exp(x: CR): CR = CR.exp(x)
    def expm1(x: CR): CR = CR.exp(CR.one) - CR.one
    def log(x: CR): CR = CR.log(x)
    def log1p(x: CR): CR = CR.log(CR.one + x)
    def pi: CR = CR.pi
    def sin(x: CR): CR = CR.sin(x)
    def sinh(x: CR): CR = CR.sinh(x)
    def tan(x: CR): CR = CR.tan(x)
    def tanh(x: CR): CR = CR.tanh(x)
    def toDegrees(a: CR): CR = a / (CR.two * CR.pi) * CR(360)
    def toRadians(a: CR): CR = a / CR(360) * (CR.two * CR.pi)

    def ceil(x: CR): CR = x.ceil
    def floor(x: CR): CR = x.floor
    def isWhole(x: CR): Boolean = x.isWhole
    def round(x: CR): CR = x.round

    def toRational(x: CR): Rational = x.toRational
    def toDouble(x: CR): Double = x.toRational.toDouble
    def toBigDecimal(x: CR): BigDecimal = x.toRational.toBigDecimal
    def toBigInt(x: CR): BigInt = x.toRational.toBigInt
    def toByte(x: CR): Byte = x.toRational.toByte
    def toFloat(x: CR): Float = x.toRational.toFloat
    def toInt(x: CR): Int = x.toRational.toInt
    def toLong(x: CR): Long = x.toRational.toLong
    def toNumber(x: CR): Number = Number(x.toRational)
    def toShort(x: CR): Short = x.toRational.toShort
    def toString(x: CR): String = x.toString

    def toType[B](x: CR)(implicit ev: ConvertableTo[B]): B =
      ev.fromRational(x.toRational)

    def fromBigDecimal(n: BigDecimal): CR = CR(n)
    def fromBigInt(n: BigInt): CR = CR(n)
    def fromByte(n: Byte): CR = CR(n)
    def fromFloat(n: Float): CR = CR(n)
    def fromLong(n: Long): CR = CR(n)
    def fromRational(n: Rational): CR = CR(n)
    def fromShort(n: Short): CR = CR(n)

    def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): CR =
      CR(ev.toRational(b))
  }
}

case class Exact(n: Rational) extends CR { x =>
  def apply(p: Int): SafeLong = CR.roundUp(Rational(2).pow(p) * n)
}

case class Inexact(f: Int => SafeLong) extends CR {
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
