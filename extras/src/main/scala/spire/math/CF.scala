package spire.math

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import spire.math.{SafeLong => Z}
import spire.math.cf.{Eval, GCF}
import CF.{Term, Infinity}
import Z.{zero, one}

/**
 * CF represents a continued fraction, which allow us to perform exact
 * real arithmetic.
 *
 * The class is based on some of Bill Gosper's algorithms dealing with
 * continued fractions. We use "simple" continued fractions, whose
 * numerators are all 1 and whose denominators all have the same sign.
 *
 * We use [a; b, c, d] notation to represent a continued fraction,
 * where this is equivalent to `a + 1 / (b + (1 / c + (1 / d)))`, i.e.
 *
 *                1
 *       a + -----------
 *                  1
 *           b + -------
 *                    1
 *               c + ---
 *                    d
 *
 * So for example, here are some representations of various rational
 * numbers as continued fractions:
 *
 *       2   [2]
 *     1/3   [0; 3]
 *     4/3   [1; 3]
 *     5/3   [1; 1, 2]
 *    17/5   [3; 2, 2]
 *
 * Every rational number can be represented as a continued fraction
 * with a finite number of terms. Every irrational number that is not
 * transcendental (e.g. sqrt(2)) can be represented as a continued
 * fraction with an infinite (but repeating) series of terms. For
 * example:
 *
 *  phi      [1; 1, ...]
 *  sqrt(2)  [1; 2, ...]
 *  sqrt(3)  [1; 1, 2, ...]
 *  sqrt(7)  [2; 1, 1, 1, 4, ...]
 *
 * Finally, transcendental numbers have an infinite, non-repeating
 * representation. For example:
 *
 *  e        [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...]
 */
sealed trait CF { lhs =>

  def fold[A](a: => A, f: (Z, () => CF) => A): A =
    this match {
      case Term(n, g) => f(n, g)
      case Infinity => a
    }

  def fold_(f: (Z, () => CF) => CF): CF =
    this match {
      case Term(n, g) => f(n, g)
      case Infinity => this
    }

  // TODO: use implicit parameter to specify approximation
  def signum: Int = {
    // FIXME: i is a similar hack to above, to support breaking out if
    // we somehow have a stream of zeros.
    @tailrec def loop(i: Int, cf: CF): Int =
      if (i > 20) 0 else cf match {
        case Term(n, f) => if (n.isZero) loop(i + 1, f()) else n.signum
        case _ => 0
      }
    loop(0, this)
  }

  // TODO: use implicit parameter to specify approximation
  def compare(rhs: CF): Int =
    (lhs - rhs).signum

  // // TODO: cooperative eq
  // override def equals(rhs: Any): Boolean =
  //   rhs match {
  //     case rhs: CF => lhs === rhs
  //     case _ => false
  //   }

  // // TODO: use implicit parameter to specify approximation
  // def ===(rhs: CF): Boolean =
  //   (lhs compare rhs) == 0

  def unary_- : CF =
    fold_((n, f) => -n ~: -f())

  def abs: CF =
    fold_((n, f) => n.abs ~: f().abs)

  def +(rhs: Long): CF =
    this + Z(rhs)

  def +(rhs: Z): CF =
    Eval.eval4(one, rhs, zero, one, this)

  def +(rhs: Rational): CF = {
    val d = rhs.denominator
    Eval.eval4(d, rhs.numerator, zero, d, this)
  }

  def +(rhs: CF): CF =
    Eval.eval8(1000, false, false, lhs, rhs,
      zero, one, one, zero,
      zero, zero, zero, one)

  def -(rhs: Long): CF =
    this - rhs

  def -(rhs: Z): CF =
    Eval.eval4(one, -rhs, zero, one, this)

  def -(rhs: Rational): CF = {
    val d = rhs.denominator
    Eval.eval4(d, -rhs.numerator, zero, d, this)
  }

  def -(rhs: CF): CF =
    Eval.eval8(1000, false, false, lhs, rhs,
      zero, one, -one, zero,
      zero, zero, zero, one)

  def *(rhs: Long): CF =
    this * Z(rhs)

  def *(rhs: Z): CF =
    Eval.eval4(rhs, zero, zero, one, this)

  def *(rhs: Rational): CF =
    Eval.eval4(rhs.numerator, zero, zero, rhs.denominator, this)

  def *(rhs: CF): CF =
    Eval.eval8(1000, false, false, lhs, rhs,
      one, zero, zero, zero,
      zero, zero, zero, one)

  def /(rhs: Long): CF =
    Eval.eval4(one, zero, zero, Z(rhs), this)

  def /(rhs: Rational): CF =
    Eval.eval4(rhs.numerator, zero, zero, rhs.denominator, this)

  def /(rhs: CF): CF =
    Eval.eval8(1000, false, false, lhs, rhs,
      zero, one, zero, zero,
      zero, zero, one, zero)

  def /~(rhs: CF): CF =
    (lhs / rhs).fold_((n, _) => CF(n))

  def %(rhs: CF): CF =
    (lhs / rhs).fold_((_, f) => zero ~: (f() * rhs))

  def /%(rhs: CF): (CF, CF) =
    (lhs / rhs).fold((Infinity, Infinity), (n, f) => (CF(n), zero ~: (f() * rhs)))

  def reciprocal: CF =
    this match {
      case Term(n, f) if n.isZero => f()
      case cf => Term(zero, () => cf)
    }

  def **(k: Int): CF =
    this pow k

  def pow(k: Int): CF = {
    def loop(b: CF, k: Int, extra: CF): CF =
      if (k == 1) b * extra
      else loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) reciprocal.pow(-k)
    else if (k == 0) CF.one
    else if (k == 1) this
    else loop(this, k - 1, this)
  }

  def toStream: Stream[Z] =
    fold(Stream.empty, (n, f) => n #:: f().toStream)

  def getString(t: Int): String = {
    def terms(t: Int, cf: CF): Stream[String] =
      cf match {
        case Term(x, f) =>
          if (t < 1) "..." #:: Stream.empty
          else x.toString #:: terms(t - 1, f())
        case Infinity =>
          Stream.empty
      }

    terms(t, this) match {
      case Stream.Empty =>
        "[∞]" // should not happen
      case w #:: Stream.Empty =>
        "[" + w + "]"
      case w #:: rest =>
        "[" + w + "; " + rest.mkString("", ", ", "]")
    }
  }

  override def toString: String =
    getString(16)
}

object CF {

  case object Infinity extends CF
  case class Term(n: Z, f: () => CF) extends CF

  implicit class CFSyntax(f: => CF) {
    def ~:(lhs: Int): CF = Term(Z(lhs), f _)
    def ~:(lhs: Long): CF = Term(Z(lhs), f _)
    def ~:(lhs: BigInt): CF = Term(Z(lhs), f _)
    def ~:(lhs: Z): CF = Term(lhs, f _)
  }

  val infinity: CF =
    Infinity

  val done: Function0[CF] =
    () => Infinity

  def apply(n: Long): CF =
    Term(Z(n), done)

  def apply(n: BigInt): CF =
    Term(Z(n), done)

  def apply(n: Z): CF =
    Term(n, done)

  def apply(s: String): CF =
    CF(Rational(s))

  def apply(r: Rational): CF = {
    def build(r: Rational): CF =
      if (r.isWhole) Term(r.toSafeLong, done) else {
        val x = r.floor
        Term(x.toSafeLong, () => build((r - x).reciprocal))
      }
    if (r.signum > 0) build(r) else if (r.signum < 0) -CF(-r) else CF.zero
  }

  val zero = 0 ~: Infinity
  val one = 1 ~: Infinity

  val e: CF = 2 ~: 1 ~: iterate(Z(2))((n, tail) => n ~: 1 ~: 1 ~: tail(), _ + 2)

  val pi: CF = GCF.toCF(GCF.pi)

  lazy val phi: CF = 1 ~: phi

  def iterate[A](a: A)(make: (A, () => CF) => CF, next: A => A): CF =
    make(a, () => iterate(next(a))(make, next))

  def repeat(digits: List[Z]): CF = {
    def loop(ns: List[Z]): CF =
      ns match {
        case n :: ns => n ~: loop(ns)
        case Nil => seq
      }
    lazy val seq = if (digits.isEmpty) Infinity else loop(digits)
    seq
  }

  def sqrtOf(r: Rational): CF =
    sqrtOf(r.numerator) / sqrtOf(r.denominator)

  def sqrtOf(n: Z): CF = {
    import spire.syntax.nroot._
    val m = n.sqrt
    val m2 = m ** 2

    // not tail-recursive, but it doesn't need to be (due to laziness)
    // as long as the code "consuming" the CF is stack-safe.
    def loop(add: Z, denom: Z, buf: ListBuffer[Z]): CF = {
      val x = (m + add) / denom
      val b = add - (x * denom)
      val denom2 = (n - b ** 2) / denom
      buf += x
      if (denom2 == 1) {
        buf += (m - b)
        x ~: (m - b) ~: repeat(buf.toList.tail)
      } else {
        x ~: loop(-b, denom2, buf)
      }
    }

    if (m2 == n) m ~: Infinity
    else loop(Z(0), Z(1), ListBuffer.empty)
  }

  def nrootOf(n: Z, k: Int): CF = {
    import spire.syntax.nroot._
    val m = n.nroot(k)
    val mk = m ** k

    // not tail-recursive, but it doesn't need to be (due to laziness)
    // as long as the code "consuming" the CF is stack-safe.
    def loop(add: Z, denom: Z, terms: List[Z]): CF = {
      val x = (m + add) / denom
      val b = add - (x * denom)
      val denom2 = (n - b ** k) / denom
      if (denom2 == 0) {
        val ts = (m - b) :: x :: terms
        x ~: (m - b) ~: repeat(ts.reverse)
      } else if (denom2 == 1) {
        val ts = (m - b) :: x :: terms
        x ~: (m - b) ~: repeat(ts.reverse)
      } else {
        val ts = x :: terms
        x ~: loop(-b, denom2, ts)
      }
    }

    if (mk == n) m ~: Infinity
    else loop(Z(0), Z(1), Nil)
  }
}
