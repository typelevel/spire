package spire.math

import scala.annotation.tailrec

case class InvalidCFError(msg: String) extends Exception(msg)

object Xyz { type Z = SafeLong }
import Xyz.Z

object Eval {

  def eval4(a: Z, b: Z, c: Z, d: Z, cf: CF): CF = {
    @tailrec def loop4(a: Z, b: Z, c: Z, d: Z, cf: CF): CF = {
      if (c == 0 && d == 0) return Infinity

      if (c != 0 && d != 0) {
        val q = a / c
        if (q == b / d) return CF.xyz(q, () => eval4(c, d, a - c*q, b - d*q, cf))
      }

      cf match {
        case CF(n, f) => loop4(a*n + b, a, c*n + d, c, f())
        case inf => loop4(a, a, c, c, inf)
      }
    }
    loop4(a, b, c, d, cf)
  }

  def eval8(
    breakout: Int, doneLeft: Boolean, doneRight: Boolean, lhs: CF, rhs: CF,
    a: Z, b: Z, c: Z, d: Z, e: Z, f: Z, g: Z, h: Z): CF = {

    @tailrec def loop8(
      i: Int, doneLeft: Boolean, doneRight: Boolean, lhs: CF, rhs: CF,
      a: Z, b: Z, c: Z, d: Z, e: Z, f: Z, g: Z, h: Z): CF = {

      if (i >= breakout || (e == 0 && f == 0 && g == 0 && h == 0)) return Infinity

      val ae = Extended(a) /~ Extended(e)
      val bf = Extended(b) /~ Extended(f)
      val cg = Extended(c) /~ Extended(g)
      val dh = Extended(d) /~ Extended(h)

      if (ae == bf && bf == cg && cg == dh) {
        val q = ae.getOrError()
        return CF.xyz(q, () => eval8(
          breakout, doneLeft, doneRight, lhs, rhs,
          e, f, g, h,
          a - e*q, b - f*q, c - g*q, d - h*q))
      }

      val xw = ((ae - cg).abs max (bf - dh).abs)
      val yw = ((ae - bf).abs max (cg - dh).abs)
      val takeLeft = doneRight || (!doneLeft && xw > yw)

      if (takeLeft) {
        lhs match {
          case CF(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, ff(), rhs,
              a*n + c, b*n + d, a, b,
              e*n + g, f*n + h, e, f)
          case inf =>
            loop8(i + 1, true, doneRight, inf, rhs,
              a, b, a, b,
              e, f, e, f)
        }
      } else {
        rhs match {
          case CF(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, lhs, ff(),
              a*n + b, a, c*n + d, c,
              e*n + f, e, g*n + h, g)
          case inf =>
            loop8(i + 1, doneLeft, true, lhs, inf,
              a, a, c, c,
              e, e, g, g)
        }
      }
    }
    loop8(0, doneLeft, doneRight, lhs, rhs, a, b, c, d, e, f, g, h)
  }
}

sealed trait CF { lhs =>

  import SafeLong.{zero, one}

  def unary_- : CF =
    this match {
      case LongTerm(n, f) => LongTerm(-n, () => -f())
      case BigTerm(n, f) => BigTerm(-n, () => -f())
      case inf => inf
    }

  def +(rhs: Long): CF = {
    val n = SafeLong(rhs)
    Eval.eval4(one, n, zero, one, this)
  }

  def +(rhs: Rational): CF = {
    val n = SafeLong(rhs.numerator)
    val d = SafeLong(rhs.denominator)
    Eval.eval4(d, n, zero, d, this)
  }

  def +(rhs: CF): CF =
    Eval.eval8(100, false, false, lhs, rhs,
      zero, one, one, zero,
      zero, zero, zero, one)

  def -(rhs: Long): CF =
    Eval.eval4(one, -SafeLong(rhs), zero, one, this)

  def -(rhs: Rational): CF = {
    val n = SafeLong(rhs.numerator)
    val d = SafeLong(rhs.denominator)
    Eval.eval4(d, -n, zero, d, this)
  }

  def -(rhs: CF): CF =
    Eval.eval8(100, false, false, lhs, rhs,
      zero, one, -one, zero,
      zero, zero, zero, one)

  def *(rhs: Long): CF =
    Eval.eval4(SafeLong(rhs), zero, zero, one, this)

  def *(rhs: Rational): CF = {
    val n = SafeLong(rhs.numerator)
    val d = SafeLong(rhs.denominator)
    Eval.eval4(n, zero, zero, d, this)
  }

  def *(rhs: CF): CF =
    Eval.eval8(100, false, false, lhs, rhs,
      one, zero, zero, zero,
      zero, zero, zero, one)

  def /(rhs: Long): CF =
    Eval.eval4(one, zero, zero, SafeLong(rhs), this)

  def /(rhs: Rational): CF = {
    val n = SafeLong(rhs.denominator)
    val d = SafeLong(rhs.numerator)
    Eval.eval4(d, zero, zero, n, this)
  }

  def /(rhs: CF): CF =
    Eval.eval8(100, false, false, lhs, rhs,
      zero, one, zero, zero,
      zero, zero, one, zero)

  def /~(rhs: CF): CF =
    (lhs / rhs) match {
      case LongTerm(n, _) => CF(n)
      case BigTerm(n, _) => CF(n)
      case inf => inf
    }

  def %(rhs: CF): CF =
    (lhs / rhs) match {
      case LongTerm(_, f) => LongTerm(0, f) * rhs
      case BigTerm(_, f) => LongTerm(0, f) * rhs
      case inf => inf
    }

  def /%(rhs: CF): (CF, CF) =
    (lhs / rhs) match {
      case LongTerm(n, f) => (CF(n), LongTerm(0, f) * rhs)
      case BigTerm(n, f) => (CF(n), LongTerm(0, f) * rhs)
      case inf => (inf, inf)
    }

  def reciprocal: CF =
    this match {
      case LongTerm(0L, f) => f()
      case BigTerm(n, f) if n == 0 => f()
      case cf => LongTerm(0L, () => cf)
    }

  def **(k: Int): CF = this pow k

  def pow(k: Int): CF = {
    def loop(b: CF, k: Int, extra: CF): CF =
      if (k == 1) b * extra
      else loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) reciprocal.pow(-k)
    else if (k == 0) CF.one
    else if (k == 1) this
    else loop(this, k - 1, this)
  }

  def toList: List[SafeLong] = {
    def unroll(cf: CF, acc: List[SafeLong]): List[SafeLong] =
      cf match {
        case CF(n, f) => unroll(f(), n :: acc)
        case _ => acc
      }
    unroll(this, Nil).reverse
  }

  def toStream: Stream[SafeLong] =
    this match {
      case CF(n, f) => n #:: f().toStream
      case _ => Stream.empty
    }

  override def toString: String = getString(10)

  def getString(t: Int): String = {
    def loop(cf: CF, t: Int): List[String] =
      if (t <= 0) "..." :: Nil else cf match {
        case LongTerm(n, f) => n.toString :: loop(f(), t - 1)
        case BigTerm(n, f) => n.toString :: loop(f(), t - 1)
        case inf => Nil
      }

    loop(this, t) match {
      case Nil => "{}"
      case h :: Nil => "{" + h + "}"
      case h :: t => "{" + h + "; " + t.mkString("", ", ", "}")
    }
  }

  def take(t: Int): CF =
    if (t <= 0) Infinity else this match {
      case LongTerm(n, f) => LongTerm(n, () => f().take(t - 1))
      case BigTerm(n, f) => BigTerm(n, () => f().take(t - 1))
      case inf => inf
    }
}

case object Infinity extends CF
case class LongTerm(n: Long, f: () => CF) extends CF
case class BigTerm(n: BigInt, f: () => CF) extends CF

object CF {

  def unapply(cf: CF): Option[(SafeLong, () => CF)] = cf match {
    case LongTerm(n, f) => Some((SafeLong(n), f))
    case BigTerm(n, f) => Some((SafeLong(n), f))
    case _ => None
  }

  implicit class CFSyntax(f: => CF) {
    def ~:(lhs: Int): CF = LongTerm(lhs, f _)
    def ~:(lhs: Long): CF = LongTerm(lhs, f _)
    def ~:(lhs: BigInt): CF = BigTerm(lhs, f _)
    def ~:(lhs: SafeLong): CF = lhs.fold(LongTerm(_, f _), BigTerm(_, f _))
  }

  val infinity: CF = Infinity

  private val done: Function0[CF] = () => Infinity

  def apply(n: Long): CF = LongTerm(n, done)
  def apply(n: BigInt): CF = BigTerm(n, done)
  def apply(n: SafeLong): CF = n.fold(apply(_), apply(_))

  def xyz(n: SafeLong, f: () => CF): CF =
    n.fold(LongTerm(_, f), BigTerm(_, f))

  val zero = 0 ~: Infinity
  val one = 1 ~: Infinity

  def apply(s: String): CF =
    s match {
      case "e" => CF.e
      case "phi" => CF.phi
      case s => CF(Rational(s))
    }

  def apply(n: Rational): CF = {
    val p = n.toBigInt
    val dd = n.numerator - p * n.denominator
    val f = if (dd == 0) done else () => apply(Rational(n.denominator, dd))
    BigTerm(p, f)
  }

  val e: CF = {
    def loop(n: SafeLong): CF = n ~: 1 ~: 1 ~: loop(n + 2)
    2 ~: 1 ~: loop(SafeLong(2))
  }

  lazy val phi: CF = 1 ~: phi
}
