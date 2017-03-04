package spire
package algebra

import java.lang.Double.{ isInfinite, isNaN, doubleToLongBits }
import java.lang.Long.{ numberOfTrailingZeros }

trait DivisionRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with MultiplicativeGroup[A] { self =>

  def fromDouble(a: Double): A = Ring.defaultFromDouble[A](a)(self, self)

}


object DivisionRing {

  @inline final def apply[A](implicit f: DivisionRing[A]): DivisionRing[A] = f

}
