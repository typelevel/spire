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
 * [1] Burnikel, Funke, Seel. Exact Geometric Computation Using Cascading. SoCG 1998.
 */
final class FPFilter[A](val approx: MaybeDouble, x: => A) {
  lazy val value: A = x
}


object FPFilter extends LowPriorityFPFilterWrappers {
  trait FPFilterEq[A] extends Eq[FPFilter[A]] {
    implicit def eq: Eq[A]

    def equiv(a: FPFilter[A], b: FPFilter[A]): Boolean = (a.approx - b.approx).sign match {
      case Some(s) => s == Zero
      case None => a.value === b.value
    }

    def nequiv(a: FPFilter[A], b: FPFilter[A]): Boolean = !equiv(a, b)
  }

  trait ConvertableToFPFilter[A] extends ConvertableTo[FPFilter[A]] {
    implicit def toA: ConvertableTo[A]

    def fromByte(a: Byte): FPFilter[A] = fromInt(a)
    def fromShort(a: Short): FPFilter[A] = fromInt(a)
    def fromInt(a: Int): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromInt(a))
    def fromLong(a: Long): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromLong(a))
    def fromFloat(a: Float): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromFloat(a))
    def fromDouble(a: Double): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromDouble(a))
    def fromBigInt(a: BigInt): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromBigInt(a))
    def fromBigDecimal(a: BigDecimal): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromBigDecimal(a))
    def fromRational(a: Rational): FPFilter[A] = new FPFilter(MaybeDouble(a), toA.fromRational(a))
  }

  trait ConvertableFromFPFilter[A] extends ConvertableFrom[FPFilter[A]] {
    implicit def fromA: ConvertableFrom[A]

    def toByte(a: FPFilter[A]): Byte = toInt(a).toByte
    def toShort(a: FPFilter[A]): Short = toInt(a).toShort
    def toInt(a: FPFilter[A]): Int = toLong(a).toInt
    def toLong(a: FPFilter[A]): Long = a.approx.toLong getOrElse (fromA.toLong(a.value))
    def toFloat(a: FPFilter[A]): Float = a.approx.toFloat getOrElse (fromA.toFloat(a.value))
    def toDouble(a: FPFilter[A]): Double = if (a.approx.isExact) {
      a.approx.approx
    } else {
      fromA.toDouble(a.value)
    }

    def toBigInt(a: FPFilter[A]): BigInt =
      a.approx.toLong map (BigInt(_)) getOrElse (fromA.toBigInt(a.value))

    def toBigDecimal(a: FPFilter[A]): BigDecimal = if (a.approx.isExact) {
      BigDecimal(a.approx.approx)
    } else {
      fromA.toBigDecimal(a.value)
    }

    def toRational(a: FPFilter[A]): Rational = if (a.approx.isExact) {
      Rational(a.approx.approx)
    } else {
      fromA.toRational(a.value)
    }

    def toString(a: FPFilter[A]): String = fromA.toString(a.value)
  }

  trait FPFilterIsRing[A] extends Ring[FPFilter[A]] {
    implicit def num: Ring[A]

    def abs(a: FPFilter[A]): FPFilter[A] = new FPFilter(a.approx.abs, num.abs(a.value))
    def negate(a: FPFilter[A]): FPFilter[A] = new FPFilter(-a.approx, -(a.value))

    def minus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx - b.approx, a.value - b.value)

    def plus(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx + b.approx, a.value + b.value)

    def times(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx * b.approx, a.value * b.value)

    def zero: FPFilter[A] = new FPFilter(MaybeDouble(0.0), num.fromInt(0))
    def one: FPFilter[A] = new FPFilter(MaybeDouble(1.0), num.fromInt(1))
  }

  trait FPFilterIsEuclideanRing[A] extends FPFilterIsRing[A] with EuclideanRing[FPFilter[A]] {
    implicit def num: EuclideanRing[A]

    def quot(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx quot b.approx, a.value /~ b.value)

    def mod(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx mod b.approx, a.value % b.value)
  }

  trait FPFilterIsField[A] extends FPFilterIsEuclideanRing[A] with Field[FPFilter[A]] {
    implicit def num: Field[A]

    def div(a: FPFilter[A], b: FPFilter[A]): FPFilter[A] =
      new FPFilter(a.approx / b.approx, a.value / b.value)
  }

  trait FPFilterIsExponential[A] extends Exponential[FPFilter[A]] {
    implicit val exp: Exponential[A]

    def pow(a: FPFilter[A], k: Int): FPFilter[A] =
      new FPFilter(a.approx pow k, a.value pow k)

    def nroot(a: FPFilter[A], n: Int): FPFilter[A] =
      new FPFilter(a.approx nroot n, a.value nroot n)
    
    override def sqrt(a: FPFilter[A]): FPFilter[A] = 
      new FPFilter(a.approx.sqrt, a.value.sqrt)
  }

  implicit def FPFilterIsRing[A](implicit ring: Ring[A]): Ring[FPFilter[A]] =
    new FPFilterIsRing[A] with ConvertableFromFPFilter[A]
                          with ConvertableToFPFilter[A]
                          with FPFilterEq[A] {
      val num = ring
      val eq = ring
      val fromA = ring
      val toA = ring
    }

  implicit def FPFilterIsEuclideanRing[A](implicit erng: EuclideanRing[A]): EuclideanRing[FPFilter[A]] =
    new FPFilterIsEuclideanRing[A] with ConvertableFromFPFilter[A]
                                   with ConvertableToFPFilter[A]
                                   with FPFilterEq[A] {
      val num = erng
      val eq = erng
      val fromA = erng
      val toA = erng
    }

  implicit def FPFilterIsField[A](implicit field: Field[A]): Field[FPFilter[A]] =
    new FPFilterIsField[A] with ConvertableFromFPFilter[A]
                           with ConvertableToFPFilter[A]
                           with FPFilterEq[A] {
      val num = field
      val eq = field
      val fromA = field
      val toA = field
    }

  implicit def FPFilterIsExponential[A](implicit e: Exponential[A]): Exponential[FPFilter[A]] = new FPFilterIsExponential[A] {
    val exp = e
  }


  /**
   * Returns the value `n` wrapped in an `FPFilter` (floating point filter).
   */
  def apply[A](n: A)(implicit f: FPFilterWrapper[A]): FPFilter[A] = f.wrap(n)
}


