package spire.math.fpf

import spire.math._
import Implicits._


/**
 * A typeclass approach to wrapping numeric types in an `FPFilter`. This way
 * was chosen, as not all conversions are lossy, so we want to be able to
 * capture those lossless conversions (`Int`, `Float`, `Double`, some `BigInt`s
 * and `Long`s, etc.), while still handling the generic case that is lossy.
 */
trait FPFilterWrapper[A] {
  def wrap(a: A): FPFilter[A]
}


trait LowPriorityFPFilterWrappers {

  /**
   * In the general case, we assume the conversion to `Double` is lossy. This
   * also makes the big assumption that if the number cannot be represented
   * approximately by a `Double`, that it'll return an `Infinity` or `NaN`
   * instead. Otherwise, the results are undefined (and most likely incorrect).
   */
  implicit def genericFPFilter[A: Ring]: FPFilterWrapper[A] = new FPFilterWrapper[A] {
    def wrap(a: A): FPFilter[A] = new FPFilter(MaybeDouble.approx(a.toDouble), a)
  }
}


/** TODO: genericFPFilter is constantly being chosen over the ones defined here.
 *        Does prioritized implicit selection work differently here for some
 *        odd reason? What the heck...
 */
object FPFilterWrapper extends LowPriorityFPFilterWrappers {
  import FPFilter._

  implicit object IntFPFilterWrapper extends FPFilterWrapper[Int] {
    def wrap(a: Int): FPFilter[Int] = new FPFilter(MaybeDouble(a), a)
  }

  implicit object FloatFPFilterWrapper extends FPFilterWrapper[Float] {
    def wrap(a: Float): FPFilter[Float] = new FPFilter(MaybeDouble(a), a)
  }

  implicit object DoubleFPFilterWrapper extends FPFilterWrapper[Double] {
    def wrap(a: Double): FPFilter[Double] = new FPFilter(MaybeDouble(a), a)
  }

  implicit object LongFPFilterWrapper extends FPFilterWrapper[Long] {
    def wrap(a: Long): FPFilter[Long] = FPFilterIsRing[Long].fromLong(a)
  }

  implicit object BigIntFPFilterWrapper extends FPFilterWrapper[BigInt] {
    def wrap(a: BigInt): FPFilter[BigInt] = FPFilterIsRing[BigInt].fromBigInt(a)
  }

  implicit object BigDecimalFPFilterWrapper extends FPFilterWrapper[BigDecimal] {
    def wrap(a: BigDecimal): FPFilter[BigDecimal] = FPFilterIsRing[BigDecimal].fromBigDecimal(a)
  }

  implicit object RationalFPFilterWrapper extends FPFilterWrapper[Rational] {
    def wrap(a: Rational): FPFilter[Rational] = FPFilterIsRing[Rational].fromRational(a)
  }
}

