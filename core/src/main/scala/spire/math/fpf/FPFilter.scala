package spire.math.fpf

import scala.math.{ max, abs }
import java.lang.Double.{ NaN, isNaN, isInfinite }

import spire.algebra._
import spire.math._


/**
 * A Floating Point Filter [1] provides a `Numeric` type that wraps another
 * `Numeric` type, but defers its computation, instead providing a floating
 * point (`Double`) approximation. For some operations, like `signum`,
 * comparisons, equality checks, toFloat, etc, the `Double` approximation may
 * be used to compute the result, rather than having to compute the exact value.
 *
 * An `FPFilter` can generally be used with any `Ring` numeric type (also
 * supports `EuclideanRing`, `Field`, and `Exponential`). However, it should be
 * kept in mind that `FPFilter` knows nothing about the type its wrapping and
 * assumes that, generally, it is more accurate than it is. When an `FPFilter`
 * cannot determine an answer to some predicate exactly, it will defer to the
 * wrapped value, so it probably doesn't make sense to wrap `Int`s, when an
 * `Int` will overflow before a `Double`!
 *
 * Good candidates to wrap in `FPFilter` are `BigInt`s, `Rational`s, and
 * `BigDecimal`s. Note that `Real` uses an `FPFilter` internally and thus
 * nothing is gained by wrapping it. The reasoning behind this is that
 * `FPFilter`s add quite a bit of space requirements, as they may hold onto the
 * entire expression tree (as call-by-name paramters), and, since `Real` also
 * does this by design, the asymptotic space requirements remain the same.
 *
 * Currently, the only way to operate on an `FPFilter` is by using its various
 * numeric typeclasses.
 *
 * Note: Don't use `FPFilter`s in hash maps. Getting the `hashCode` will always
 *       force the evaluation of `value`.
 *
 * [1] Burnikel, Funke, Seel. Exact Geometric Computation Using Cascading. SoCG 1998.
 */
final class FPFilter[A](val approx: MaybeDouble, x: => A) {
  lazy val value: A = x

  /**
   * This will always evalute the underlying `value` and will lead to serious
   * performance problems if used often.
   */
  override def hashCode: Int = value.##

  /**
   * Returns true if these values are equal. Note that this will only return
   * `true` on a successful comparison of `this.value == that.value`. However,
   * it may return `false` based on only the floating point approximations. If
   * you wish to take advantage of the case where `this.approx == that.approx`
   * exactly, you'll need to use the `Eq` type class instead.
   */
  override def equals(that: Any): Boolean = that match {
    case that: FPFilter[_] => (this.approx - that.approx).sign match {
      case Some(Negative) | Some(Positive) => false
      case _ => this.value == that.value   // This actually differs from Eq[FPFitler[A]].
    }
    case _ => false
  }
}

private[fpf] trait FPFilterEq[A] extends Eq[FPFilter[A]] {
  implicit def order: Eq[A]

  def eqv(a: FPFilter[A], b: FPFilter[A]): Boolean = (a.approx - b.approx).sign match {
    case Some(s) => s == Zero
    case None => order.eqv(a.value, b.value)
  }
}

private[fpf] trait FPFilterOrder[A] extends Order[FPFilter[A]] with FPFilterEq[A] {
  implicit def order: Order[A]

  override def eqv(a: FPFilter[A], b: FPFilter[A]): Boolean =
    super[FPFilterEq].eqv(a, b)

  def compare(a: FPFilter[A], b: FPFilter[A]): Int = (a.approx - b.approx).sign match {
    case Some(Positive) => 1
    case Some(Negative) => -1
    case Some(Zero) => 0
    case None => order.compare(a.value, b.value)
  }
}

private[fpf] trait ConvertableToFPFilter[A] extends ConvertableTo[FPFilter[A]] {
  implicit def ev: ConvertableTo[A]

  def fromByte(a: Byte): FPFilter[A] = fromInt(a)
  def fromShort(a: Short): FPFilter[A] = fromInt(a)
  def fromInt(a: Int): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromInt(a))
  def fromLong(a: Long): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromLong(a))
  def fromFloat(a: Float): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromFloat(a))
  def fromDouble(a: Double): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromDouble(a))
  def fromBigInt(a: BigInt): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromBigInt(a))
  def fromBigDecimal(a: BigDecimal): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromBigDecimal(a))
  def fromRational(a: Rational): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromRational(a))

  def fromType[B:ConvertableFrom](a: B): FPFilter[A] = sys.error("fixme")
}

private[fpf] trait ConvertableFromFPFilter[A] extends ConvertableFrom[FPFilter[A]] {
  implicit def ev: ConvertableFrom[A]

  def toByte(a: FPFilter[A]): Byte = toInt(a).toByte
  def toShort(a: FPFilter[A]): Short = toInt(a).toShort
  def toInt(a: FPFilter[A]): Int = toLong(a).toInt
  def toLong(a: FPFilter[A]): Long = a.approx.toLong getOrElse (ev.toLong(a.value))
  def toFloat(a: FPFilter[A]): Float = a.approx.toFloat getOrElse (ev.toFloat(a.value))
  def toDouble(a: FPFilter[A]): Double = if (a.approx.isExact) {
    a.approx.approx
  } else {
    ev.toDouble(a.value)
  }

  def toBigInt(a: FPFilter[A]): BigInt =
    a.approx.toLong map (BigInt(_)) getOrElse (ev.toBigInt(a.value))

  def toBigDecimal(a: FPFilter[A]): BigDecimal = if (a.approx.isExact) {
    BigDecimal(a.approx.approx)
  } else {
    ev.toBigDecimal(a.value)
  }

  def toRational(a: FPFilter[A]): Rational = if (a.approx.isExact) {
    Rational(a.approx.approx)
  } else {
    ev.toRational(a.value)
  }

  def toNumber(a: FPFilter[A]): Number = if (a.approx.isExact) {
    Number(a.approx.approx)
  } else {
    Number(ev.toBigDecimal(a.value))
  }

  def toType[B:ConvertableTo](a: FPFilter[A]) = sys.error("fixme")

  def toString(a: FPFilter[A]): String = ev.toString(a.value)
}

private[fpf] trait FPFilterIsSigned[A] extends Signed[FPFilter[A]] {
  implicit def ev: Signed[A]

  def abs(a: FPFilter[A]): FPFilter[A] = new FPFilter(a.approx.abs, ev.abs(a.value))

  def signum(a: FPFilter[A]): Int = a.approx.sign.getOrElse(ev.sign(a.value)).toInt
}

private[fpf] trait FPFilterIsRing[A] extends Ring[FPFilter[A]] {
  implicit def ev: Ring[A]

  def negate(a: FPFilter[A]): FPFilter[A] = new FPFilter(-a.approx, ev.negate(a.value))

  override def minus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx - b.approx, ev.minus(a.value, b.value))

  def plus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx + b.approx, ev.plus(a.value, b.value))

  override def pow(a: FPFilter[A], k: Int): FPFilter[A] =
    new FPFilter(a.approx pow k, ev.pow(a.value, k))

  override def times(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx * b.approx, ev.times(a.value, b.value))

  def zero: FPFilter[A] = new FPFilter(MaybeDouble(0.0), ev.fromInt(0))
  def one: FPFilter[A] = new FPFilter(MaybeDouble(1.0), ev.fromInt(1))

  override def fromInt(a: Int): FPFilter[A] = new FPFilter(MaybeDouble(a), ev.fromInt(a))
}

private[fpf] trait FPFilterIsEuclideanRing[A] extends FPFilterIsRing[A] with EuclideanRing[FPFilter[A]] with FPFilterEq[A] {
  implicit def ev: EuclideanRing[A]

  def quot(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx quot b.approx, ev.quot(a.value, b.value))

  def mod(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx mod b.approx, ev.mod(a.value, b.value))

  def gcd(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    euclid(a, b)(this)
}

private[fpf] trait FPFilterIsField[A] extends FPFilterIsEuclideanRing[A] with Field[FPFilter[A]] {
  implicit def ev: Field[A]

  def div(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
    new FPFilter(a.approx / b.approx, ev.div(a.value, b.value))
}

private[fpf] trait FPFilterIsNRoot[A] extends NRoot[FPFilter[A]] {
  implicit def ev: NRoot[A]

  def nroot(a: FPFilter[A], n: Int): FPFilter[A] =
    new FPFilter(a.approx nroot n, ev.nroot(a.value, n))
  
  override def sqrt(a: FPFilter[A]): FPFilter[A] = 
    new FPFilter(a.approx.sqrt, ev.sqrt(a.value))

  def fpow(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] = sys.error("fixme")
}

private[fpf] trait FPFilterIsNumeric[A] extends Numeric[FPFilter[A]]
with FPFilterIsField[A] with FPFilterIsNRoot[A]
with FPFilterOrder[A] with FPFilterIsSigned[A]
with ConvertableFromFPFilter[A] with ConvertableToFPFilter[A] {
  implicit val ev: Numeric[A]
  def order = ev

  def isWhole(a: FPFilter[A]): Boolean = eqv(quot(a, one), zero)
  def ceil(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  def floor(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  def round(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  override def fromInt(n: Int): FPFilter[A] = super[ConvertableToFPFilter].fromInt(n)
  override def fromDouble(n: Double): FPFilter[A] = super[ConvertableToFPFilter].fromDouble(n)
}

private[fpf] trait FPFilterIsFractional[A] extends Fractional[FPFilter[A]]
with FPFilterIsField[A] with FPFilterIsNRoot[A]
with FPFilterOrder[A] with FPFilterIsSigned[A]
with ConvertableFromFPFilter[A] with ConvertableToFPFilter[A] {
  implicit val ev: Fractional[A]
  def order = ev

  def isWhole(a: FPFilter[A]): Boolean = eqv(quot(a, one), zero)
  def ceil(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  def floor(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  def round(a: FPFilter[A]): FPFilter[A] = sys.error("fixme")
  override def fromInt(n: Int): FPFilter[A] = super[ConvertableToFPFilter].fromInt(n)
  override def fromDouble(n: Double): FPFilter[A] = super[ConvertableToFPFilter].fromDouble(n)
}


private[fpf] trait LowPriorityFPFilterImplicits {
  implicit def FPFilterIsNumeric[A](implicit num: Numeric[A]): Numeric[FPFilter[A]] =
    new FPFilterIsNumeric[A] {
      val ev = num
    }

  implicit def FPFilterIsEuclideanRing[A](implicit erng: EuclideanRing[A], eq0: Eq[A]): EuclideanRing[FPFilter[A]] =
    new FPFilterIsEuclideanRing[A] {
      val ev = erng
      val order = eq0
    }
}

object FPFilter extends LowPriorityFPFilterImplicits with LowPriorityFPFilterWrappers {

  implicit def FPFilterIsFractional[A]
  (implicit num: Fractional[A]): Fractional[FPFilter[A]] =
    new FPFilterIsFractional[A] {
      val ev = num
    }

  /**
   * Returns the value `n` wrapped in an `FPFilter` (floating point filter).
   */
  def apply[A](n: A)(implicit f: FPFilterWrapper[A]): FPFilter[A] = f.wrap(n)
}
