package numerics.math.fpf

import scala.math.{ max, abs }
import java.lang.Double.{ NaN, isNaN, isInfinite }

import numerics.math._
import Implicits._


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
  override def hashCode: Int = value ##

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


object FPFilter extends LowPriorityFPFilterWrappers {
  trait FPFilterEq[A] extends Eq[FPFilter[A]] {
    implicit def ev: Eq[A]

    def eq(a: FPFilter[A], b: FPFilter[A]): Boolean = (a.approx - b.approx).sign match {
      case Some(s) => s == Zero
      case None => a.value === b.value
    }

    def neq(a: FPFilter[A], b: FPFilter[A]): Boolean = !eq(a, b)
  }

  trait ConvertableToFPFilter[A] extends ConvertableTo[FPFilter[A]] {
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
  }

  trait ConvertableFromFPFilter[A] extends ConvertableFrom[FPFilter[A]] {
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

    def toString(a: FPFilter[A]): String = ev.toString(a.value)
  }

  trait FPFilterIsRing[A] extends Ring[FPFilter[A]] {
    implicit def ev: Ring[A]

    def abs(a: FPFilter[A]): FPFilter[A] = new FPFilter(a.approx.abs, ev.abs(a.value))
    def negate(a: FPFilter[A]): FPFilter[A] = new FPFilter(-a.approx, -(a.value))

    def minus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx - b.approx, a.value - b.value)

    def plus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx + b.approx, a.value + b.value)

    def pow(a: FPFilter[A], k: Int): FPFilter[A] =
      new FPFilter(a.approx pow k, a.value pow k)

    def times(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx * b.approx, a.value * b.value)

    def zero: FPFilter[A] = new FPFilter(MaybeDouble(0.0), ev.fromInt(0))
    def one: FPFilter[A] = new FPFilter(MaybeDouble(1.0), ev.fromInt(1))
  }

  trait FPFilterIsEuclideanRing[A] extends FPFilterIsRing[A] with EuclideanRing[FPFilter[A]] {
    implicit def ev: EuclideanRing[A]

    def quot(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx quot b.approx, a.value /~ b.value)

    def mod(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx mod b.approx, a.value % b.value)
  }

  trait FPFilterIsField[A] extends FPFilterIsEuclideanRing[A] with Field[FPFilter[A]] {
    implicit def ev: Field[A]

    def div(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx / b.approx, a.value / b.value)
  }

  trait FPFilterIsEuclideanRingWithNRoot[A]
  extends FPFilterIsEuclideanRing[A] with EuclideanRingWithNRoot[FPFilter[A]] {
    implicit val ev: EuclideanRingWithNRoot[A]

    def nroot(a: FPFilter[A], n: Int): FPFilter[A] =
      new FPFilter(a.approx nroot n, a.value nroot n)
    
    override def sqrt(a: FPFilter[A]): FPFilter[A] = 
      new FPFilter(a.approx.sqrt, a.value.sqrt)
  }

  trait FPFilterIsFieldWithNRoot[A] extends FPFilterIsField[A] with FieldWithNRoot[FPFilter[A]] {
    implicit val ev: FieldWithNRoot[A]

    def nroot(a: FPFilter[A], n: Int): FPFilter[A] =
      new FPFilter(a.approx nroot n, a.value nroot n)
    
    override def sqrt(a: FPFilter[A]): FPFilter[A] = 
      new FPFilter(a.approx.sqrt, a.value.sqrt)
  }


  implicit def FPFilterIsRing[A](implicit ring: Ring[A]): Ring[FPFilter[A]] =
    new FPFilterIsRing[A] with ConvertableFromFPFilter[A]
                          with ConvertableToFPFilter[A]
                          with FPFilterEq[A] {
      val ev = ring
    }

  implicit def FPFilterIsEuclideanRing[A](implicit erng: EuclideanRing[A]): EuclideanRing[FPFilter[A]] =
    new FPFilterIsEuclideanRing[A] with ConvertableFromFPFilter[A]
                                   with ConvertableToFPFilter[A]
                                   with FPFilterEq[A] {
      val ev = erng
    }

  implicit def FPFilterIsField[A](implicit field: Field[A]): Field[FPFilter[A]] =
    new FPFilterIsField[A] with ConvertableFromFPFilter[A]
                           with ConvertableToFPFilter[A]
                           with FPFilterEq[A] {
      val ev = field
    }

  implicit def FPFilterIsFieldWithNRoot[A]
  (implicit e: FieldWithNRoot[A]): FieldWithNRoot[FPFilter[A]] =
    new FPFilterIsFieldWithNRoot[A] with ConvertableFromFPFilter[A]
                                    with ConvertableToFPFilter[A]
                                    with FPFilterEq[A] {
      val ev = e
    }

  implicit def FPFilterIsEuclideanRingWithNRoot[A]
  (implicit e: EuclideanRingWithNRoot[A]): EuclideanRingWithNRoot[FPFilter[A]] =
    new FPFilterIsEuclideanRingWithNRoot[A] with ConvertableFromFPFilter[A]
                                    with ConvertableToFPFilter[A]
                                    with FPFilterEq[A] {
      val ev = e
    }



  /**
   * Returns the value `n` wrapped in an `FPFilter` (floating point filter).
   */
  def apply[A](n: A)(implicit f: FPFilterWrapper[A]): FPFilter[A] = f.wrap(n)
}


