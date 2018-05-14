package spire.math

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

/**
 * Implementation of extended real numbers.
 *
 * This type wraps an existing number type (A), adding three new values:
 *
 *  - PosInf(): a value greater than all real numbers
 *  - MinusInf(): a value less than all real numbers
 *  - Undef(): an undefined value used for error-handling.
 *
 * These values may be familiar: they parallel the three sentinel
 * values provided by floating point (+inf, -inf, and nan).
 *
 * The Finite(a) constructor wraps "normal" A values.
 *
 * Operations on Finite values behave as normal, except for division
 * by zero which produces Undef() instead of an error. (Note that this
 * is different behavior than in floating point, where division by
 * zero will often produce infinities.)
 *
 * Operations involving Undef will always produce Undef.
 *
 * Here are the rules governing PosInf (+∞) and MinusInf (-∞),
 * assuming that (x, y, z) are arbitrary Finite values:
 *
 *    -(∞)     = (-∞)
 *    ∞ + x    = ∞
 *    ∞ + (-∞) = undef
 *    ∞ * x    = ∞ if x > 0
 *    ∞ * x    = -∞ if x < 0
 *    ∞ * 0    = undef
 *    ∞ * ±∞   = ±∞
 *    x / ∞    = 0
 *    ∞ / x    = ∞ if x != 0
 *    ∞ / 0    = undef
 *    ∞ / ∞    = undef
 *
 * If other values are needed, methods such as .undefTo and .fold can
 * be used to translate an Extended[A] value to another form. For
 * example, if your domain requires (+∞) * 0 = 0, you could say:
 *
 *   (x * y).undefToZero
 *
 * When ConvertableFrom[A] exists (i.e. if A can be converted to a
 * Double) .toDouble can convert Extended[A] values to Doubles.
 */
sealed abstract class Extended[A] { lhs =>

  import Double.{PositiveInfinity, NegativeInfinity, NaN}
  import Extended.{zero, one, minusOne, inf, PlusInf, Finite, MinusInf, Undef}

  /**
   * Catamorphism for Extended[A].
   *
   * This method corresponds to a pattern-match on Extended[A]: the
   * arguments to this method handle all the "cases" that would be
   * covered.
   */
  def fold[B](undef: => B, neg: => B, f: A => B, pos: => B): B =
    this match {
      case Finite(n) => f(n)
      case PlusInf() => pos
      case MinusInf() => neg
      case Undef() => undef
    }

  /**
   * Limited fold.
   *
   * This covers a common case of fold, where the result type is
   * Extended[A] and the Undef() case remains undefined.
   *
   * TODO: Come up with a better name than fold_.
   */
  def fold_(neg: => Extended[A], f: A => Extended[A], pos: => Extended[A]): Extended[A] =
    this match {
      case Finite(n) => f(n)
      case PlusInf() => pos
      case MinusInf() => neg
      case Undef() => this
    }

  /**
   * Return Some(a) for finite values, None otherwise.
   */
  def toOption: Option[A] =
    fold(None, None, n => Some(n), None)

  /**
   * Return a finite value, throw an exception otherwise.
   */
  def getOrError(): A =
    this match {
      case Finite(a) => a
      case _ => throw new ArithmeticException(toString)
    }

  /**
   * Throw an exception on undefined values, return otherwise.
   */
  def undefToError(): Extended[A] =
    this match {
      case Undef() => throw new ArithmeticException(toString)
      case x => x
    }

  /**
   * Replace undefined values with zeros.
   */
  def undefToZero(implicit ev: AdditiveMonoid[A]): Extended[A] =
    this match {
      case Undef() => zero
      case x => x
    }

  /**
   * Replace undefined values with the given value.
   */
  def undefTo(ext: Extended[A]): Extended[A] =
    this match {
      case Undef() => ext
      case x => x
    }

  /**
   * Convert an Extended[A] to a Double.
   */
  def toDouble(implicit ev: ConvertableFrom[A]): Double =
    fold(NaN, NegativeInfinity, _.toDouble, PositiveInfinity)

  def ===(rhs: Extended[A])(implicit ev: Eq[A]): Boolean =
    lhs match {
      case Finite(n1) => rhs.fold(false, false, _ === n1, false)
      case Undef() => false
      case inf => inf == rhs
    }

  def =!=(rhs: Extended[A])(implicit ev: Eq[A]): Boolean =
    !(lhs === rhs)

  /**
   * (Partial) comparator for Extended[A] values.
   *
   * Since Undef() is not a numeric value, it has no place in a total
   * ordering. Thus, this is a partial ordering (it is almost total).
   *
   * As long as you have defined values, this method behaves exactly
   * like a .compare method would.
   */
  def partialCompare(rhs: Extended[A])(implicit ev: PartialOrder[A]): Double =
    lhs match {
      case Finite(n1) => rhs.fold(NaN, -1.0, n1 partialCompare _, 1.0)
      case PlusInf() => rhs.fold(NaN, 1.0, _ => 1.0, 0.0)
      case MinusInf() => rhs.fold(NaN, 0.0, _ => -1.0, -1.0)
      case Undef() => NaN
    }

  def <(rhs: Extended[A])(implicit ev: PartialOrder[A]): Boolean =
    (lhs partialCompare rhs) < 0.0
  def <=(rhs: Extended[A])(implicit ev: PartialOrder[A]): Boolean =
    (lhs partialCompare rhs) <= 0.0
  def >(rhs: Extended[A])(implicit ev: PartialOrder[A]): Boolean =
    (lhs partialCompare rhs) > 0.0
  def >=(rhs: Extended[A])(implicit ev: PartialOrder[A]): Boolean =
    (lhs partialCompare rhs) >= 0.0

  /**
   * Absolute value.
   *
   * For negative values, this method returns their positive
   * equivalent.
   */
  def abs(implicit ev: AdditiveGroup[A], s: Signed[A]): Extended[A] =
    fold_(PlusInf(), n => Finite(n.abs), lhs)

  /**
   * Return the maximum of the given values.
   *
   * If either value is undefined, Undef() is returned.
   */
  def max(rhs: Extended[A])(implicit ev: Order[A]): Extended[A] =
    lhs match {
      case Finite(n1) => rhs.fold_(lhs, n2 => Finite(n1 max n2), rhs)
      case PlusInf() => rhs.fold_(lhs, n2 => lhs, lhs)
      case MinusInf() => rhs
      case Undef() => lhs
    }

  /**
   * Return the minimum of the given values.
   *
   * If either value is undefined, Undef() is returned.
   */
  def min(rhs: Extended[A])(implicit ev: Order[A]): Extended[A] =
    lhs match {
      case Finite(n1) => rhs.fold_(rhs, n2 => Finite(n1 min n2), lhs)
      case MinusInf() => rhs.fold_(lhs, n2 => lhs, lhs)
      case PlusInf() => rhs
      case Undef() => lhs
    }

  /**
   * Negate the given value.
   *
   * Extended behavior:
   *    -(undef) = undef
   *    -(∞) = (-∞)
   *    -(-∞) = ∞
   */
  def unary_-(implicit ev: AdditiveGroup[A]): Extended[A] =
    fold_(PlusInf(), n => Finite(-n), MinusInf())

  /**
   * Return the sign of the number as a Double.
   *
   * For undefined values, NaN is returned.
   */
  def signum(implicit s: Signed[A]): Double =
    fold(NaN, -1.0, _.signum.toDouble, 1.0)

  /**
   * Return true if the value is zero, false otherwise.
   */
  def isZero(implicit s: Signed[A]): Boolean =
    fold(false, false, _.signum == 0, false)

  def isDefined: Boolean =
    this != Undef()

  def isUndefined: Boolean =
    this == Undef()

  /**
   * Add two values together.
   *
   * Extended behavior:
   *    undef + _ = undef
   *    ∞ + x     = ∞
   *    ∞ + (-∞)  = undef
   */
  def +(rhs: Extended[A])(implicit ev: AdditiveMonoid[A]): Extended[A] =
    lhs match {
      case Finite(n1) =>
        rhs.fold_(rhs, n2 => Finite(n1 + n2), rhs)
      case PlusInf() =>
        rhs.fold_(Undef(), _ => lhs, lhs)
      case MinusInf() =>
        rhs.fold_(lhs, _ => lhs, Undef())
      case Undef() =>
        lhs
    }

  /**
   * Subtract one value from another.
   *
   * (x - y) is equivalent to (x + (-y)).
   */
  def -(rhs: Extended[A])(implicit ev: AdditiveGroup[A]): Extended[A] =
    lhs + (-rhs)

  /**
   * Subtract one value from another.
   *
   * Extended behavior:
   *    undef * _ = undef
   *    ∞ * x     = ∞ if x > 0
   *    ∞ * x     = -∞ if x < 0
   *    ∞ * 0     = undef
   *    ∞ * ±∞    = ±∞
   */
  def *(rhs: Extended[A])(implicit ev: MultiplicativeSemigroup[A], s: Signed[A]): Extended[A] =
    lhs match {
      case Finite(n1) =>
        rhs.fold_(inf(-n1.signum), n2 => Finite(n1 * n2), inf(n1.signum))
      case PlusInf() =>
        rhs.fold_(rhs, n2 => inf(n2.signum), lhs)
      case MinusInf() =>
        rhs.fold_(PlusInf(), n2 => inf(-n2.signum), lhs)
      case Undef() =>
        this
    }

  /**
   * Quotient operator.
   *
   * Extended behavior:
   *    undef /~ _ = undef
   *    _ /~ undef = undef
   *    ∞ /~ x     = ∞ if x > 0
   *    ∞ /~ x     = -∞ if x < 0
   *    ∞ /~ 0     = undef
   *    ∞ /~ ∞     = undef
   */
  def /~(rhs: Extended[A])(implicit ev: EuclideanRing[A], s: Signed[A]): Extended[A] =
    rhs match {
      case Finite(n2) =>
        val x = n2.signum
        if (x == 0) Undef() else lhs.fold_(inf(-x), n1 => Finite(ev.equot(n1, n2)), inf(x))
      case Undef() =>
        this
      case _ =>
        lhs.fold_(Undef(), _ => zero, Undef())
    }

  /**
   * Reciprocal operator.
   *
   * Equivalent to (1/x).
   */
  def reciprocal(implicit ev: Field[A], s: Signed[A]): Extended[A] =
    lhs match {
      case Finite(n) => if (n.signum == 0) Undef() else Finite(n.reciprocal)
      case Undef() => this
      case _ => zero
    }

  /**
   * Division operator.
   *
   * Extended behavior:
   *    undef / _ = undef
   *    _ / undef = undef
   *    ∞ / x     = ∞ if x > 0
   *    ∞ / x     = -∞ if x < 0
   *    ∞ / 0     = undef
   *    ∞ / ∞     = undef
   */
  def /(rhs: Extended[A])(implicit ev: Field[A], s: Signed[A]): Extended[A] =
    rhs match {
      case Finite(n2) =>
        val x = n2.signum
        if (x == 0) Undef() else lhs.fold_(inf(-x), n1 => Finite(n1 / n2), inf(x))
      case Undef() =>
        rhs
      case _ =>
        lhs.fold_(Undef(), _ => zero, Undef())
    }

  /**
   * Exponentiation operator.
   *
   * Extended behavior:
   *    undef ** k = undef
   *    ±∞ ** 0    = undef
   *    ∞ ** k     = ∞
   *    (-∞) ** k  = -∞ if k is odd
   *    (-∞) ** k  = ∞ if k > 0 and k is even
   *    (-∞) ** k  = undef if k < 0 and k is even
   */
  def **(k: Int)(implicit ev: NRoot[A], r: Ring[A]): Extended[A] =
    lhs match {
      case Finite(n) =>
        if (k > 1) Finite(n pow k)
        else if (k == 0) one
        else if (k == 1) lhs
        else if (k == Int.MinValue) throw new ArithmeticException(s"invalid exponent: $k")
        else Finite(n nroot -k)
      case PlusInf() =>
        if (k != 0) lhs else Undef()
      case MinusInf() =>
        if (k == 0) Undef()
        else if ((k & 1) == 1) lhs
        else if (k > 0) PlusInf()
        else Undef()
      case Undef() =>
        lhs
    }

  /**
   * Alias for **.
   */
  def pow(k: Int)(implicit ev: NRoot[A], r: Ring[A]): Extended[A] =
    this ** k

  /**
   * Find the kth-root.
   *
   * Equivalent to (x ** -k), ignoring the Int.MinValue case.
   */
  def nroot(k: Int)(implicit ev: NRoot[A], r: Ring[A]): Extended[A] =
    lhs match {
      case Finite(n) =>
        if (k > 1) Finite(n nroot k)
        else if (k == 0) one
        else if (k == 1) lhs
        else if (k == Int.MinValue) throw new ArithmeticException(s"invalid exponent: $k")
        else Finite(n pow -k)
      case PlusInf() =>
        if (k != 0) lhs else Undef()
      case Undef() =>
        lhs
      case MinusInf() =>
        if (k == 0) Undef()
        else if ((k & 1) == 1) lhs
        else if (k < 0) PlusInf()
        else Undef()
    }

  /**
   * Display the extended value as a String.
   */
  override def toString: String =
    fold("∅", "-∞", _.toString, "+∞")
}

object Extended /*extends ExtendedInstances*/ {

  case class Undef[A]() extends Extended[A]
  case class PlusInf[A]() extends Extended[A]
  case class Finite[A](a: A) extends Extended[A]
  case class MinusInf[A]() extends Extended[A]

  def zero[A](implicit ev: AdditiveMonoid[A]): Extended[A] =
    Finite(ev.zero)

  def one[A](implicit ev: MultiplicativeMonoid[A]): Extended[A] =
    Finite(ev.one)

  def minusOne[A](implicit ev: Ring[A]): Extended[A] =
    Finite(-ev.one)

  /**
   * Construct a finite Extended[A] value.
   */
  def apply[A](a: A): Finite[A] =
    Finite(a)

  /**
   * Construct an "infinite" Extended[A] value with the appropriate
   * sign:
   *
   *     sign > 0: PlusInf()
   *     sign < 0: MinusInf()
   *     sign = 0: Undef()
   */
  def inf[A](sign: Int): Extended[A] =
    if (sign > 0) PlusInf() else if (sign < 0) MinusInf() else Undef()
}

// trait ExtendedInstances extends ExtendedInstances1 {
// 
//   /**
//    * Partial ordering for Extended[A].
//    *
//    * This is not a total ordering because Undef() does not have a
//    * location on the extended real number line.
//    */
//   def partialOrder[A: PartialOrder]: PartialOrder[Extended[A]] =
//     new PartialOrder[Extended[A]] {
//       def partialCompare(x: Extended[A], y: Extended[A]): Double =
//         x partialCompare y
//     }
// 
//   /**
//    * Partial field for Extended[A].
//    *
//    * This structure satisfies the laws when results are defined.
//    * however, for all operations involving Undef (and some involving
//    * infinities) it is not law-abiding.
//    */
//   def partialField[A: Field: Signed]: Field[Extended[A]] =
//     new ExtendedField[A] {
//       def algebra: Field[A] = implicitly
//       def signed: Signed[A] = implicitly
//     }
// }
// 
// trait ExtendedInstances1 extends ExtendedInstances2 {
// 
//   /**
//    * Equality for Extended[A].
//    */
//   implicit def extendedEq[A: Eq]: Eq[Extended[A]] =
//     new Eq[Extended[A]] {
//       def eqv(x: Extended[A], y: Extended[A]): Boolean = x === y
//     }
// 
//   /**
//    * Partial euclidean ring for Extended[A].
//    *
//    * This structure satisfies the laws when results are defined.
//    * however, for all operations involving Undef (and some involving
//    * infinities) it is not law-abiding.
//    */
//   def partialEuclideanRing[A: EuclideanRing: Signed]: EuclideanRing[Extended[A]] =
//     new ExtendedEuclideanRing[A] {
//       def algebra: EuclideanRing[A] = implicitly
//       def signed: Signed[A] = implicitly
//     }
// }
// 
// trait ExtendedInstances2 extends ExtendedInstances3 {
// 
//   /**
//    * Partial ring for Extended[A].
//    *
//    * This structure satisfies the laws when results are defined.
//    * however, for all operations involving Undef (and some involving
//    * infinities) it is not law-abiding.
//    */
//   def partialRing[A: Ring: Signed]: Ring[Extended[A]] =
//     new ExtendedRing[A] {
//       def algebra: Ring[A] = implicitly
//       def signed: Signed[A] = implicitly
//     }
// }
// 
// trait ExtendedInstances3 {
// 
//   /**
//    * Partial semiring for Extended[A].
//    *
//    * This structure satisfies the laws when results are defined.
//    * however, for all operations involving Undef (and some involving
//    * infinities) it is not law-abiding.
//    */
//   def partialSemiring[A: Semiring: Signed]: Semiring[Extended[A]] =
//     new ExtendedSemiring[A] {
//       def algebra: Semiring[A] = implicitly
//       def signed: Signed[A] = implicitly
//     }
// 
// }
// 
// trait ExtendedSemiring[A] extends Semiring[Extended[A]] {
//   implicit def signed: Signed[A]
//   implicit def algebra: Semiring[A]
//   def zero: Extended[A] = Extended.zero
//   def plus(x: Extended[A], y: Extended[A]): Extended[A] = x + y
//   def times(x: Extended[A], y: Extended[A]): Extended[A] = x * y
// }
// 
// trait ExtendedRing[A] extends Ring[Extended[A]] {
//   implicit def signed: Signed[A]
//   implicit def algebra: Ring[A]
//   def zero: Extended[A] = Extended.zero
//   def one: Extended[A] = Extended.one
//   def negate(x: Extended[A]): Extended[A] = -x
//   def plus(x: Extended[A], y: Extended[A]): Extended[A] = x + y
//   def times(x: Extended[A], y: Extended[A]): Extended[A] = x * y
// }
// 
// trait ExtendedEuclideanRing[A] extends EuclideanRing[Extended[A]] with ExtendedRing[A] {
//   implicit def algebra: EuclideanRing[A]
//   def quot(x: Extended[A], y: Extended[A]): Extended[A] = x /~ y
//   def mod(x: Extended[A], y: Extended[A]): Extended[A] = ???
//   def gcd(x: Extended[A], y: Extended[A]): Extended[A] = ???
// }
// 
// trait ExtendedField[A] extends Field[Extended[A]] with ExtendedEuclideanRing[A] {
//   implicit def algebra: Field[A]
//   def div(x: Extended[A], y: Extended[A]): Extended[A] = x / y
// }
