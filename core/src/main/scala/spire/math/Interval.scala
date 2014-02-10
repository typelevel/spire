package spire.math

import spire.algebra._
import spire.implicits._
import spire.math.poly.Term

import Predef.{any2stringadd => _, _}

/**
 * Interval represents a set of values, usually numbers.
 * 
 * Intervals have upper and lower bounds. Each bound can be one of
 * three kinds:
 * 
 *   * Closed: The boundary value is included in the interval.
 *   * Open: The boundary value is excluded from the interval.
 *   * Unbound: There is no boundary value.
 *
 * When the underlying type of the interval supports it, intervals may
 * be used in arithmetic. There are several possible interpretations
 * of interval arithmetic: the interval can represent uncertainty
 * about a single value (for instance, a quantity +/- tolerance in
 * engineering) or it can represent all values in the interval
 * simultaneously. In this implementation we have chosen to use the
 * probabillistic interpretation.
 *
 * One common pitfall with interval arithmetic is that many familiar
 * algebraic relations do not hold. For instance, given two intervals
 * a and b:
 * 
 *   a == b does not imply a * a == a * b
 *
 * Consider a = b = [-1, 1]. Since any number times itself is
 * non-negative, a * a = [0, 1]. However, a * b = [-1, 1], since we
 * may actually have a=1 and b=-1.
 *
 * These situations will result in loss of precision (in the form of
 * wider intervals). The result is not wrong per se, but less
 * acccurate than it could be.
 */

sealed abstract class Interval[A](implicit order: Order[A]) { lhs =>

  @inline private[this] final def isClosed(flags: Int): Boolean = flags == 0
  @inline private[this] final def isClosedBelow(flags: Int): Boolean = (flags & 1) == 0
  @inline private[this] final def isClosedAbove(flags: Int): Boolean = (flags & 2) == 0

  @inline private[this] final def isOpen(flags: Int): Boolean = flags == 3
  @inline private[this] final def isOpenBelow(flags: Int): Boolean = (flags & 1) == 1
  @inline private[this] final def isOpenAbove(flags: Int): Boolean = (flags & 2) == 1

  @inline private[this] final def lowerFlag(flags: Int): Int = flags & 1
  @inline private[this] final def upperFlag(flags: Int): Int = flags & 2

  private[this] final def lowerFlagToUpper(flags: Int): Int = (flags & 1) << 1
  private[this] final def upperFlagToLower(flags: Int): Int = (flags & 2) >>> 1

  @inline private[this] final def swapFlags(flags: Int): Int =
    ((flags & 1) << 1) | ((flags & 2) >>> 1)

  def isEmpty: Boolean = this match {
    case Ranged(lower, upper, flags) => isOpen(flags) && lower === upper
    case _ => false
  }

  def contains(t: A): Boolean =
    isAtOrBelow(t) && isAtOrAbove(t)

  def crosses(t: A): Boolean =
    isBelow(t) && isAbove(t)

  def crossesZero(implicit ev: AdditiveMonoid[A]): Boolean =
    isBelow(ev.zero) && isAbove(ev.zero)

  private[spire] def lowerPair: Option[(A, Int)] = this match {
    case Ranged(lower, upper, flags) => Some((lower, lowerFlag(flags)))
    case Above(lower, flags) => Some((lower, flags))
    case _ => None
  }

  private[spire] def upperPair: Option[(A, Int)] = this match {
    case Ranged(lower, upper, flags) => Some((upper, upperFlag(flags)))
    case Above(upper, flags) => Some((upper, flags))
    case _ => None
  }

  private[spire] def upperPairAsLower: Option[(A, Int)] = this match {
    case Ranged(lower, upper, flags) => Some((upper, upperFlagToLower(flags)))
    case Above(upper, flags) => Some((upper, upperFlagToLower(flags)))
    case _ => None
  }

  private[this] def lowerPairBelow(lower1: A, flags1: Int, lower2: A, flags2: Int): Boolean =
    lower1 < lower2 || lower1 === lower2 && (isClosedBelow(flags1) || isOpenBelow(flags2))

  private[this] def upperPairAbove(upper1: A, flags1: Int, upper2: A, flags2: Int): Boolean =
    upper1 > upper2 || upper1 === upper2 && (isClosedAbove(flags1) || isOpenAbove(flags2))

  def isSupersetOf(rhs: Interval[A]): Boolean = lhs match {
    case All() =>
      true
    case Above(lower1, flags1) =>
      rhs.lowerPair.map { case (lower2, flags2) =>
        lowerPairBelow(lower1, flags1, lower2, flags2)
      }.getOrElse(false)
    case Below(upper1, flags1) =>
      rhs.upperPair.map { case (upper2, flags2) =>
        upperPairAbove(upper1, flags1, upper2, flags2)
      }.getOrElse(false)
    case Ranged(lower1, upper1, flags1) =>
      rhs match {
        case Ranged(lower2, upper2, flags2) =>
          lowerPairBelow(lower1, flags1, lower2, flags2) &&
          upperPairAbove(upper1, flags1, upper2, flags2)
        case _ =>
          false
      }
  }

  def isSubsetOf(rhs: Interval[A]): Boolean =
    rhs.isSupersetOf(lhs)

  def isAbove(t: A): Boolean = this match {
    case Below(upper, flags) => upper > t
    case Ranged(lower, upper, flags) => upper > t
    case _ => true
  }

  def isBelow(t: A): Boolean = this match {
    case Above(lower, flags) => lower < t
    case Ranged(lower, upper, flags) => lower < t
    case _ => true
  }

  def isAtOrAbove(t: A) = this match {
    case Below(upper, flags) =>
      upper > t || isClosedAbove(flags) && upper === t
    case Ranged(lower, upper, flags) =>
      upper > t || isClosedAbove(flags) && upper === t
    case _ =>
      true
  }

  def isAtOrBelow(t: A) = this match {
    case Above(lower, flags) =>
      lower < t || isClosedBelow(flags) && lower === t
    case Ranged(lower, upper, flags) =>
      lower < t || isClosedBelow(flags) && lower === t
    case _ =>
      true
  }

  def isAt(t: A) = this match {
    case Ranged(lower, upper, flags) =>
      isClosed(flags) && lower === t && t === upper
    case _ =>
      false
  }

  private[this] def minLower(lower1: A, lower2: A, flags1: Int, flags2: Int): (A, Int) =
    (lower1 compare lower2) match {
      case -1 => (lower1, flags1)
      case 0 => (lower1, flags1 & flags2)
      case 1 => (lower2, flags2)
    }

  private[this] def maxLower(lower1: A, lower2: A, flags1: Int, flags2: Int): (A, Int) =
    (lower1 compare lower2) match {
      case -1 => (lower2, flags2)
      case 0 => (lower1, flags1 | flags2)
      case 1 => (lower1, flags1)
    }

  private[this] def minUpper(upper1: A, upper2: A, flags1: Int, flags2: Int): (A, Int) =
    (upper1 compare upper2) match {
      case -1 => (upper1, flags1)
      case 0 => (upper1, flags1 | flags2)
      case 1 => (upper2, flags2)
    }

  private[this] def maxUpper(upper1: A, upper2: A, flags1: Int, flags2: Int): (A, Int) =
    (upper1 compare upper2) match {
      case -1 => (upper2, flags2)
      case 0 => (upper1, flags1 & flags2)
      case 1 => (upper1, flags1)
    }

  def intersects(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Boolean =
    !(lhs intersect rhs).isEmpty

  def &(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] =
    lhs intersect rhs

  def intersect(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] = lhs match {
    case All() => rhs
    case Below(upper1, flags1) => rhs match {
      case All() =>
        lhs
      case Below(upper2, flags2) =>
        val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
        Below(u, uf)
      case Above(lower2, flags2) =>
        Interval.withFlags(lower2, upper1, flags1 | flags2)
      case Ranged(lower2, upper2, flags2) =>
        val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
        Interval.withFlags(lower2, u, lowerFlag(flags2) | uf)
    }
    case Above(lower1, flags1) => rhs match {
      case All() =>
        lhs
      case Above(lower2, flags2) =>
        val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
        Above(l, lf)
      case Below(upper2, flags2) =>
        Interval.withFlags(lower1, upper2, flags1 | flags2)
      case Ranged(lower2, upper2, flags2) =>
        val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
        Interval.withFlags(l, upper2, lf | upperFlag(flags2))
    }
    case Ranged(lower1, upper1, flags1) =>
      rhs match {
        case All() =>
          lhs
        case Above(lower2, flags2) =>
          val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
          Interval.withFlags(l, upper1, lf | upperFlag(flags1))
        case Below(upper2, flags2) =>
          val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
          Interval.withFlags(lower1, u, lowerFlag(flags1) | uf)
        case Ranged(lower2, upper2, flags2) =>
          val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
          val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
          Interval.withFlags(l, u, lf | uf)
      }
  }

  def split(t: A)(implicit r: AdditiveMonoid[A]): (Interval[A], Interval[A]) =
    (this intersect Interval.below(t), this intersect Interval.above(t))

  def splitAtZero(implicit ev: AdditiveMonoid[A]): (Interval[A], Interval[A]) =
    split(ev.zero)

  def mapAroundZero[B](f: Interval[A] => B)(implicit ev: AdditiveMonoid[A]): (B, B) =
    splitAtZero match {
      case (a, b) => (f(a), f(b))
    }

  def |(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] =
    lhs union rhs

  def union(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] =
    (lhs, rhs) match {
      case (All(), _) => lhs
      case (_, All()) => rhs
      case (Above(_, _), Below(_, _)) => All()
      case (Below(_, _), Above(_, _)) => All()

      case (Below(upper1, flags1), Below(upper2, flags2)) =>
        val (u, uf) = maxUpper(upper1, upper2, flags1, flags2)
        Below(u, uf)
      case (Below(upper1, flags1), Ranged(_, upper2, flags2)) =>
        val (u, uf) = maxUpper(upper1, upper2, flags1, flags2)
        Below(u, uf)
      case (Ranged(_, upper1, flags1), Below(upper2, flags2)) =>
        val (u, uf) = maxUpper(upper1, upper2, flags1, flags2)
        Below(u, uf)

      case (Above(lower1, flags1), Above(lower2, flags2)) =>
        val (l, lf) = minLower(lower1, lower2, flags1, flags2)
        Above(l, lf)
      case (Above(lower1, flags1), Ranged(lower2, _, flags2)) =>
        val (l, lf) = minLower(lower1, lower2, flags1, flags2)
        Above(l, lf)
      case (Ranged(lower1, _, flags1), Above(lower2, flags2)) =>
        val (l, lf) = minLower(lower1, lower2, flags1, flags2)
        Above(l, lf)

      case (Ranged(lower1, upper1, flags1), Ranged(lower2, upper2, flags2)) =>
        val (l, lf) = minLower(lower1, lower2, flags1, flags2)
        val (u, uf) = maxUpper(upper1, upper2, flags1, flags2)
        Interval.withFlags(l, u, lf | uf)
    }

  override def toString(): String = this match {
    case All() =>
      "(-∞, ∞)"
    case Above(lower, flags) =>
      if (isClosedBelow(flags)) s"[$lower, ∞)" else s"($lower, ∞)"
    case Below(upper, flags) =>
      if (isClosedAbove(flags)) s"(-∞, $upper]" else s"(-∞, $upper)"
    case Ranged(lower, upper, flags) =>
      if (lower === upper) {
        if (isClosed(flags)) s"[$lower]" else "(Ø)"
      } else {
        val s1 = if (isClosedBelow(flags)) s"[$lower" else s"($lower"
        val s2 = if (isClosedAbove(flags)) s"$upper]" else s"$upper)"
        s"$s1, $s2"
      }
  }

  def abs(implicit m: AdditiveGroup[A]): Interval[A] =
    if (crossesZero) {
      this match {
        case Ranged(lower, upper, fs) =>
          val x = -lower
          if (x > upper) Ranged(m.zero, x, lowerFlagToUpper(fs))
          else if (upper > x) Ranged(m.zero, upper, upperFlag(fs))
          else Ranged(m.zero, x, lowerFlagToUpper(fs) & upperFlag(fs))
        case _ =>
          Interval.atOrAbove(m.zero)
      }
    } else if (isBelow(m.zero)) {
      -this
    } else {
      this
    }

  def min(rhs: Interval[A])(implicit m: AdditiveMonoid[A]): Interval[A] = {
    def zzz(x1: A, f1: Int, x2: A, f2: Int, f3: Int): (A, Int) =
      if (x1 < x2) (x1, f1) else if (x2 < x1) (x2, f2) else (x1, f3)

    val lower = for {
      (x1, f1) <- lhs.lowerPair
      (x2, f2) <- rhs.lowerPair
    } yield zzz(x1, f1, x2, f2, f1 & f2)

    val upper = (lhs.upperPair, rhs.upperPair) match {
      case (None, p2) => p2
      case (p1, None) => p1
      case (Some((x1, f1)), Some((x2, f2))) => Some(zzz(x1, f1, x2, f2, f1 | f2))
    }

    fromOptionalTpls(lower, upper)
  }

  def max(rhs: Interval[A])(implicit m: AdditiveMonoid[A]): Interval[A] = {
    def zzz(x1: A, f1: Int, x2: A, f2: Int, f3: Int): (A, Int) =
      if (x1 > x2) (x1, f1) else if (x2 > x1) (x2, f2) else (x1, f3)

    val lower = (lhs.lowerPair, rhs.lowerPair) match {
      case (None, p2) => p2
      case (p1, None) => p1
      case (Some((x1, f1)), Some((x2, f2))) => Some(zzz(x1, f1, x2, f2, f1 | f2))
    }

    val upper = for {
      (x1, f1) <- lhs.upperPair
      (x2, f2) <- rhs.upperPair
    } yield zzz(x1, f1, x2, f2, f1 & f2)

    fromOptionalTpls(lower, upper)
  }

  def combine(rhs: Interval[A])(f: (A, A) => A): Interval[A] = {
    val ll: Option[(A, Int)] = for {
      (x1, f1) <- lhs.lowerPair
      (x2, f2) <- rhs.lowerPair
    } yield (f(x1, x2), f1 | f2)

    val uu: Option[(A, Int)] = for {
      (x1, f1) <- lhs.upperPair
      (x2, f2) <- rhs.upperPair
    } yield (f(x1, x2), f1 | f2)

    (ll, uu) match {
      case (Some((l, lf)), Some((u, uf))) => Ranged(l, u, lf | uf)
      case (None, Some((u, uf))) => Below(u, uf)
      case (Some((l, lf)), None) => Above(l, lf)
      case (None, None) => All()
    }
  }

  def +(rhs: Interval[A])(implicit ev: AdditiveSemigroup[A]): Interval[A] =
    combine(rhs)(_ + _)

  def -(rhs: Interval[A])(implicit ev: AdditiveGroup[A]): Interval[A] = {
    val ll: Option[(A, Int)] = for {
      (x1, f1) <- lhs.lowerPair
      (x2, f2) <- rhs.upperPair
    } yield (x1 - x2, f1 | upperFlagToLower(f2))

    val uu: Option[(A, Int)] = for {
      (x1, f1) <- lhs.upperPair
      (x2, f2) <- rhs.lowerPair
    } yield (x1 - x2, f1 | lowerFlagToUpper(f2))

    (ll, uu) match {
      case (Some((l, lf)), Some((u, uf))) => Ranged(l, u, lf | uf)
      case (None, Some((u, uf))) => Below(u, uf)
      case (Some((l, lf)), None) => Above(l, lf)
      case (None, None) => All()
    }

  }

  private[this] def minTpl(t1: (A, Int), t2: (A, Int)): (A, Int) =
    if (t1._1 < t2._1) t1 else t2

  private[this] def maxTpl(t1: (A, Int), t2: (A, Int)): (A, Int) =
    if (t1._1 > t2._1) t1 else t2

  private[this] def fromOptionalTpls(t1: Option[(A, Int)], t2: Option[(A, Int)])(implicit r: AdditiveMonoid[A]): Interval[A] =
    (t1, t2) match {
      case (None, None) => Interval.all
      case (Some((x1, f1)), None) => Above(x1, f1)
      case (None, Some((x2, f2))) => Below(x2, f2)
      case (Some(t1), Some(t2)) => fromTpls(t1, t2)
    }

  private[this] def fromTpls(t1: (A, Int), t2: (A, Int))(implicit r: AdditiveMonoid[A]): Interval[A] =
    Interval.withFlags(t1._1, t2._1, lowerFlag(t1._2) | lowerFlagToUpper(t2._2))

  def *(rhs: Interval[A])(implicit ev: Semiring[A]): Interval[A] = {
    if (lhs.isEmpty || rhs.isEmpty) return Interval.empty[A]
    val z = ev.zero
    if (lhs.isAt(z) || rhs.isAt(z)) return Interval.point(z)

    lhs match {
      case All() => lhs

      case Above(lower1, lf1) => rhs match {
        case All() =>
          rhs
        case Above(lower2, lf2) =>
          if (lower1 < z || lower2 < z) All()
          else Above(lower1 * lower2, lf1 | lf2)
        case Below(upper2, uf2) =>
          if (lower1 < z || upper2 > z) All()
          else Below(lower1 * upper2, lowerFlagToUpper(lf1) | uf2)
        case Ranged(lower2, upper2, flags2) =>
          if (rhs.crossesZero) All()
          else if (rhs.isAbove(z)) Above(lower1 * lower2, lf1 | lowerFlag(flags2))
          else Below(lower1 * upper2, lowerFlagToUpper(lf1) | upperFlag(flags2))
      }

      case Below(upper1, uf1) => rhs match {
        case All() =>
          rhs
        case Above(lower2, lf2) =>
          if (upper1 > z || lower2 < z) All()
          else Below(upper1 * lower2, uf1 | lowerFlagToUpper(lf2))
        case Below(upper2, uf2) =>
          if (upper1 > z || upper2 > z) All()
          else Above(upper1 * upper2, upperFlagToLower(uf1) | upperFlagToLower(uf2))
        case Ranged(lower2, upper2, flags2) =>
          if (rhs.crossesZero) All()
          else if (rhs.isAbove(z)) Below(upper1 * lower2, uf1 | lowerFlagToUpper(flags2))
          else Above(upper1 * lower2, upperFlagToLower(uf1) | lowerFlag(flags2))
      }

      case Ranged(lower1, upper1, flags1) => rhs match {
        case All() =>
          rhs
        case Above(lower2, lf2) =>
          if (lower2 < z) All()
          else Above(lower1 * lower2, lowerFlag(flags1) | lf2)
        case Below(upper2, uf2) =>
          if (upper2 > z) All()
          else Below(lower1 * upper2, lowerFlagToUpper(flags1) | uf2)
        case Ranged(lower2, upper2, flags2) =>
          val ll = (lower1 * lower2, lowerFlag(flags1) | lowerFlag(flags2))
          val lu = (lower1 * upper2, lowerFlag(flags1) | upperFlagToLower(flags2))
          val ul = (upper1 * lower2, upperFlagToLower(flags1) | lowerFlag(flags2))
          val uu = (upper1 * upper2, upperFlagToLower(flags1) | upperFlagToLower(flags2))
          val lcz = lhs.crossesZero
          val rcz = rhs.crossesZero
          if (lcz && rcz) {
            fromTpls(minTpl(lu, ul), maxTpl(ll, uu))
          } else if (lcz) {
            if (rhs.isAbove(z)) fromTpls(lu, uu) else fromTpls(ul, ll)
          } else if (rcz) {
            if (lhs.isAbove(z)) fromTpls(ul, uu) else fromTpls(lu, ll)
          } else if (lhs.isBelow(z) == rhs.isBelow(z)) {
            fromTpls(minTpl(ll, uu), maxTpl(ll, uu))
          } else {
            fromTpls(minTpl(lu, ul), maxTpl(lu, ul))
          }
      }
    }
  }

  def reciprocal(implicit ev: Field[A]): Interval[A] = {
    val z = ev.zero
    def error = throw new java.lang.ArithmeticException("/ by zero")
    if (contains(z)) return error

    this match {
      case All() =>
        error

      case Above(lower, lf) =>
        if (lower === z) this
        else Ranged(z, lower.reciprocal, 1 | lowerFlagToUpper(lf))

      case Below(upper, uf) =>
        if (upper === z) this
        else Ranged(upper.reciprocal, z, 2 | upperFlagToLower(uf))

      case Ranged(lower, upper, flags) =>
        if (lower === z) Above(upper.reciprocal, upperFlagToLower(flags))
        else if (upper === z) Below(lower.reciprocal, lowerFlagToUpper(flags))
        else Ranged(upper.reciprocal, lower.reciprocal, swapFlags(flags))
    }
  }

  def /(rhs: Interval[A])(implicit ev: Field[A]): Interval[A] =
    lhs * rhs.reciprocal

  // def baddiv(rhs: Interval[A])(implicit ev: Field[A]): Interval[A] = {
  //   val z = ev.zero
  //   def err = throw new java.lang.ArithmeticException("/ by zero")
  //   rhs match {
  //     case All() => err
  //     case Above(lower2, lf2) if (lower2 <= z) => err
  //     case Below(upper2, uf2) if (z <= upper2) => err
  //     case Ranged(lower2, upper2, flags2) if (lower2 <= z && z <= upper2) => err
  // 
  //     case Above(lower2, lf2) => lhs match {
  //       case All() =>
  //         lhs
  //       case Above(lower1, lf1) =>
  //         Interval.above(z)
  //       case Below(upper1, uf1) =>
  //         Interval.below(z)
  //       case Ranged(lower1, upper1, flags1) =>
  //         if (lower1 < z) {
  //           Ranged(lower1 / lower2, upper1 / lower2, flags1 | lf2 | lowerFlagToUpper(lf2))
  //         } else {
  //           Ranged(z, upper1 / lower2, 1 | upperFlag(flags1) | lowerFlagToUpper(lf2))
  //         }
  //     }
  // 
  //     case Below(upper2, uf2) => lhs match {
  //       case All() =>
  //         lhs
  //       case Above(lower1, lf1) =>
  //         Interval.below(z)
  //       case Below(upper1, uf1) =>
  //         Interval.above(z)
  //       case Ranged(lower1, upper1, flags1) =>
  //         if (lower1 < z) {
  //           Ranged(upper1 / upper2, lower1 / upper2, swapFlags(flags1) | uf2 | upperFlagToLower(uf2))
  //         } else {
  //           Ranged(z, lower1 / upper2, 2 | lowerFlagToUpper(flags1) | uf2)
  //         }
  //     }
  // 
  //     case Ranged(lower2, upper2, flags2) =>
  //       if (lower2 > z) {
  //         // positive denominator
  //         lhs match {
  //           case All() =>
  //             lhs
  //           case Above(lower1, lf1) =>
  //             if (lower1 >= z) {
  //               Above(lower1 / upper2, lf1 | upperFlagToLower(flags2))
  //             } else {
  //               Above(lower1 / lower2, lf1 | lowerFlag(flags2))
  //             }
  //           case Below(upper1, uf1) =>
  //             if (upper1 > z) {
  //               Below(upper1 / lower2, uf1 | lowerFlagToUpper(flags2))
  //             } else {
  //               Below(upper1 / upper2, uf1 | upperFlag(flags2))
  //             }
  //           case Ranged(lower1, upper1, flags1) =>
  //             if (lower1 >= z) {
  //               // positive / positive
  //               Ranged(lower1 / upper2, upper1 / lower2, flags1 | swapFlags(flags2))
  //             } else if (upper1 > z) {
  //               // both / positive
  //               Ranged(lower1 / lower2, upper1 / lower2, flags1 | lowerFlag(flags2) | lowerFlagToUpper(flags2))
  //             } else {
  //               // negative / positive
  //               Ranged(lower1 / lower2, upper1 / upper2, flags1 | flags2)
  //             }
  //         }
  //       } else {
  //         // negative denominator, since denominator interval can't cross zero
  //         lhs match {
  //           case All() =>
  //             lhs
  // 
  //           case Above(lower1, lf1) =>
  //             val uf1x = lowerFlagToUpper(lf1)
  //             if (lower1 > z)
  //               Below(lower1 / upper2, uf1x | upperFlag(flags2))
  //             else
  //               Below(lower1 / lower2, uf1x | lowerFlagToUpper(flags2))
  // 
  //           case Below(upper1, uf1) =>
  //             val lf1x = upperFlagToLower(uf1)
  //             if (upper1 > z)
  //               Above(upper1 / upper2, lf1x | upperFlagToLower(flags2))
  //             else
  //               Above(upper1 / lower2, lf1x | lowerFlag(flags2))
  // 
  //           case Ranged(lower1, upper1, flags1) =>
  //             if (lower1 >= z) {
  //               // positive / negative [0,a,b] / [c,d,0] = [b/d,a/c,0]
  //               Ranged(upper1 / upper2, lower1 / lower2, swapFlags(flags1 | flags2))
  //             } else if (upper1 > z) {
  //               // both / negative [a,0,b] / [c,d,0] = [b/d,0,a/d]
  //               Ranged(upper1 / upper2, lower1 / upper2, swapFlags(flags1) | lowerFlag(flags2) | lowerFlagToUpper(flags2))
  //             } else {
  //               // negative / negative [a,b,0] / [c,d,0] = [0,b/c,a/d]
  //               Ranged(upper1 / lower2, lower1 / upper2, swapFlags(flags1) | flags2)
  //             }
  //         }
  //       }
  //   }
  // }

  def +(rhs: A)(implicit ev: AdditiveSemigroup[A]): Interval[A] =
    this match {
      case Ranged(l, u, flags) => Ranged(l + rhs, u + rhs, flags)
      case Above(l, lf) => Above(l + rhs, lf)
      case Below(u, uf) => Below(u + rhs, uf)
      case All() => this
    }

  def -(rhs: A)(implicit ev: AdditiveGroup[A]): Interval[A] =
    this + (-rhs)

  def unary_-()(implicit ev: AdditiveGroup[A]): Interval[A] =
    this match {
      case Ranged(l, u, f) => Ranged(-u, -l, swapFlags(f))
      case Above(l, lf) => Above(-l, lowerFlagToUpper(lf))
      case Below(u, uf) => Below(-u, upperFlagToLower(uf))
      case All() => this
    }

  def *(rhs: A)(implicit ev: Semiring[A]): Interval[A] =
    if (rhs < ev.zero) {
      this match {
        case Ranged(l, u, f) => Ranged(u * rhs, l * rhs, swapFlags(f))
        case Above(l, lf) => Above(l * rhs, lowerFlagToUpper(lf))
        case Below(u, uf) => Below(u * rhs, upperFlagToLower(uf))
        case All() => this
      }
    } else if (rhs === ev.zero) {
      Interval.zero
    } else {
      this match {
        case Ranged(l, u, flags) => Ranged(l * rhs, u * rhs, flags)
        case Above(l, lf) => Above(l * rhs, lf)
        case Below(u, uf) => Below(u * rhs, uf)
        case All() => this
      }
    }

  def pow(k: Int)(implicit r: Ring[A]): Interval[A] = {
    def loop(b: Interval[A], k: Int, extra: Interval[A]): Interval[A] =
      if (k == 1)
        b * extra
      else
        loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) {
      throw new IllegalArgumentException(s"negative exponent: $k")
    } else if (k == 0) {
      Interval.point(r.one)
    } else if (k == 1) {
      this
    } else {
      loop(this, k - 1, this)
    }
  }

  def nroot(k: Int)(implicit r: Ring[A], n: NRoot[A]): Interval[A] = {
    if (k == 1) {
      this
    } else if ((k & 1) == 0 && isBelow(r.zero)) {
      sys.error("can't take even root of negative number")
    } else {
      this match {
        case All() => this
        case Above(l, lf) => Above(l.nroot(k), lf)
        case Below(u, uf) => Below(u.nroot(k), uf)
        case Ranged(l, u, flags) => Ranged(l.nroot(k), u.nroot(k), flags)
      }
    }
  }

  def sqrt(implicit r: Ring[A], n: NRoot[A]): Interval[A] = nroot(2)

  def top(epsilon: A)(implicit r: AdditiveGroup[A]): Option[A] = this match {
    case Below(upper, uf) =>
      Some(if (isOpenAbove(uf)) upper - epsilon else upper)
    case Ranged(_, upper, flags) =>
      Some(if (isOpenAbove(flags)) upper - epsilon else upper)
    case _ =>
      None
  }

  def bottom(min: A, epsilon: A)(implicit r: AdditiveGroup[A]): Option[A] = this match {
    case Above(lower, lf) =>
      Some(if (isOpenBelow(lf)) lower + epsilon else lower)
    case Ranged(lower, _, flags) =>
      Some(if (isOpenBelow(flags)) lower + epsilon else lower)
    case _ =>
      None
  }

  import spire.random.{Dist, Uniform}

  def dist(min: A, max: A, epsilon: A)(implicit u: Uniform[A], r: AdditiveGroup[A]): Dist[A] =
    u(bottom(min, epsilon).getOrElse(min), top(epsilon).getOrElse(max))

  def translate(p: Polynomial[A])(implicit ev: Field[A]): Interval[A] = {
    val terms2 = p.terms.map { case Term(c, e) => Term(Interval.point(c), e) }
    val p2 = Polynomial(terms2)
    p2(this)
  }

  import Interval.{Bound, Open, Closed, Unbound}

  def lowerBound: Bound[A] = lowerPair match {
    case Some((a, n)) => if (isOpenBelow(n)) Open(a) else Closed(a)
    case None => Unbound()
  }

  def upperBound: Bound[A] = upperPair match {
    case Some((a, n)) => if (isOpenAbove(n)) Open(a) else Closed(a)
    case None => Unbound()
  }

  def mapBounds[B: Order: AdditiveMonoid](f: A => B): Interval[B] =
    Interval.fromBounds(lowerBound.map(f), upperBound.map(f))

  def fold[B](f: (Bound[A], Bound[A]) => B): B =
    f(lowerBound, upperBound)
}

case class All[A: Order] private[spire] () extends Interval[A]
case class Above[A: Order] private[spire] (lower: A, flags: Int) extends Interval[A]
case class Below[A: Order] private[spire] (upper: A, flags: Int) extends Interval[A]
case class Ranged[A: Order] private[spire] (lower: A, upper: A, flags: Int) extends Interval[A]

object Interval {

  sealed trait Bound[A] { lhs =>
    def map[B](f: A => B): Bound[B] = this match {
      case Open(a) => Open(f(a))
      case Closed(a) => Closed(f(a))
      case Unbound() => Unbound()
    }
    def combine[B](rhs: Bound[A])(f: (A, A) => A): Bound[A] = (lhs, rhs) match {
      case (Unbound(), _) => lhs
      case (_, Unbound()) => rhs
      case (Closed(a), y) => y.map(b => f(a, b))
      case (x, Closed(b)) => x.map(a => f(a, b))
      case (Open(a), Open(b)) => Open(f(a, b))
    }

    def unary_-()(implicit ev: AdditiveGroup[A]): Bound[A] =
      lhs.map(-_)
    def reciprocal()(implicit ev: MultiplicativeGroup[A]): Bound[A] =
      lhs.map(_.reciprocal)

    def +(a: A)(implicit ev: AdditiveSemigroup[A]): Bound[A] = map(_ + a)
    def -(a: A)(implicit ev: AdditiveGroup[A]): Bound[A] = map(_ - a)
    def *(a: A)(implicit ev: MultiplicativeSemigroup[A]): Bound[A] = map(_ * a)
    def /(a: A)(implicit ev: MultiplicativeGroup[A]): Bound[A] = map(_ / a)

    def +(rhs: Bound[A])(implicit ev: AdditiveSemigroup[A]): Bound[A] =
      lhs.combine(rhs)(_ + _)
    def -(rhs: Bound[A])(implicit ev: AdditiveGroup[A]): Bound[A] =
      lhs.combine(rhs)(_ - _)
    def *(rhs: Bound[A])(implicit ev: MultiplicativeSemigroup[A]): Bound[A] =
      lhs.combine(rhs)(_ * _)
    def /(rhs: Bound[A])(implicit ev: MultiplicativeGroup[A]): Bound[A] =
      lhs.combine(rhs)(_ / _)
  }

  case class Unbound[A]() extends Bound[A]
  case class Open[A](a: A) extends Bound[A]
  case class Closed[A](a: A) extends Bound[A]

  private[spire] def withFlags[A: Order: AdditiveMonoid](lower: A, upper: A, flags: Int): Interval[A] =
    if (lower <= upper) Ranged(lower, upper, flags) else Interval.empty[A]

  def empty[A](implicit o: Order[A], r: AdditiveMonoid[A]): Interval[A] =
    Ranged(r.zero, r.zero, 3)

  def point[A: Order](a: A): Interval[A] =
    Ranged(a, a, 0)

  def zero[A](implicit o: Order[A], r: Semiring[A]): Interval[A] =
    Ranged(r.zero, r.zero, 0)

  def all[A: Order]: Interval[A] = All[A]()

  def apply[A: Order: AdditiveMonoid](lower: A, upper: A): Interval[A] = closed(lower, upper)

  def fromBounds[A: Order: AdditiveMonoid](lower: Bound[A], upper: Bound[A]): Interval[A] =
    (lower, upper) match {
      case (Closed(x), Closed(y)) => closed(x, y)
      case (Open(x), Open(y)) => open(x, y)
      case (Unbound(), Open(y)) => below(y)
      case (Open(x), Unbound()) => above(x)
      case (Unbound(), Closed(y)) => atOrBelow(y)
      case (Closed(x), Unbound()) => atOrAbove(x)
      case (Closed(x), Open(y)) => openAbove(x, y)
      case (Open(x), Closed(y)) => openBelow(x, y)
      case (Unbound(), Unbound()) => all
    }

  def closed[A: Order: AdditiveMonoid](lower: A, upper: A): Interval[A] =
    if (lower <= upper) Ranged(lower, upper, 0) else Interval.empty[A]
  def open[A: Order: AdditiveMonoid](lower: A, upper: A): Interval[A] =
    if (lower <= upper) Ranged(lower, upper, 3) else Interval.empty[A]
  def openBelow[A: Order: AdditiveMonoid](lower: A, upper: A): Interval[A] =
    if (lower < upper) Ranged(lower, upper, 1) else Interval.empty[A]
  def openAbove[A: Order: AdditiveMonoid](lower: A, upper: A): Interval[A] =
    if (lower < upper) Ranged(lower, upper, 2) else Interval.empty[A]

  def above[A: Order](a: A): Interval[A] = Above(a, 1)
  def below[A: Order](a: A): Interval[A] = Below(a, 2)
  def atOrAbove[A: Order](a: A): Interval[A] = Above(a, 0)
  def atOrBelow[A: Order](a: A): Interval[A] = Below(a, 0)

  implicit def eq[A: Eq]: Eq[Interval[A]] =
    new Eq[Interval[A]] {
      def eqv(x: Interval[A], y: Interval[A]): Boolean =
        (x.lowerPair === y.lowerPair) && (x.upperPair === y.upperPair)
    }

  implicit def semiring[A](implicit ev: Semiring[A], o: Order[A]): Semiring[Interval[A]] =
    new Semiring[Interval[A]] {
      def zero: Interval[A] = Interval.point(ev.zero)
      def plus(x: Interval[A], y: Interval[A]): Interval[A] = x + y
      def times(x: Interval[A], y: Interval[A]): Interval[A] = x * y
    }
}
