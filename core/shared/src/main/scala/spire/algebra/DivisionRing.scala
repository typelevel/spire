package spire
package algebra

import java.lang.Double.{ isInfinite, isNaN, doubleToLongBits }
import java.lang.Long.{ numberOfTrailingZeros }

trait DivisionRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with MultiplicativeGroup[A] { self =>

  def fromDouble(a: Double): A = DivisionRing.fromDouble[A](a)(self, self)
}


object DivisionRing {

  @inline final def apply[A](implicit f: DivisionRing[A]): DivisionRing[A] = f

  // TODO: use instead algebra Ring.fromDouble, when algebra > 0.6.0 is published
  /**
    * This is implemented in terms of basic ops. However, this is
    * probably significantly less efficient than can be done with a specific
    * type. So, it is recommended to specialize this general method.
    *
    * This is possible because a Double is a rational number.
    */
  def fromDouble[A](a: Double)(implicit ringA: Ring[A], mgA: MultiplicativeGroup[A]): A =
    if (a == 0.0) ringA.zero else {
      import ringA._
      require(!isInfinite(a) && !isNaN(a),
        "Double must be representable as a fraction.")
      val bits = doubleToLongBits(a)
      val m = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L
      val zeros = numberOfTrailingZeros(m)
      val value = m >>> zeros
      val exp = ((bits >> 52) & 0x7FF).toInt - 1075 + zeros // 1023 + 52

      val high = times(fromInt((value >>> 30).toInt), fromInt(1 << 30))
      val low = fromInt((value & 0x3FFFFFFF).toInt)
      val num = plus(high, low)
      val unsigned = if (exp > 0) {
        times(num, pow(fromInt(2), exp))
      } else if (exp < 0) {
        mgA.div(num, pow(fromInt(2), -exp))
      } else {
        num
      }

      if (a < 0) negate(unsigned) else unsigned
    }

}
