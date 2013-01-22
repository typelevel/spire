package spire.algebra

import spire.math._

import scala.{ specialized => spec }

/**
 * A simple class for numeric types that are a subset of the reals.
 */
trait IsReal[@spec A] {
  def toDouble(a: A): Double
}

object IsReal {
  def apply[@spec A](implicit A: IsReal[A]): IsReal[A] = A

  private def apply[@spec A](f: A => Double) = new IsReal[A] {
    def toDouble(a: A) = f(a)
  }

  implicit val ByteIsReal = IsReal[Byte] { x: Byte => x.toDouble }
  implicit val ShortIsReal = IsReal[Short] { x: Short => x.toDouble }
  implicit val IntIsReal = IsReal[Int] { x: Int => x.toDouble }
  implicit val LongIsReal = IsReal[Long] { x: Long => x.toDouble }
  implicit val BigIntIsReal = IsReal[BigInt] { x: BigInt => x.toDouble }
  implicit val UByteIsReal = IsReal[UByte] { x: UByte => x.toDouble }
  implicit val UShortIsReal = IsReal[UShort] { x: UShort => x.toDouble }
  implicit val UIntIsReal = IsReal[UInt] { x: UInt => x.toDouble }
  implicit val ULongIsReal = IsReal[ULong] { x: ULong => x.toDouble }
  implicit val FloatIsReal = IsReal[Float] { x: Float => x.toDouble }
  implicit val DoubleIsReal = IsReal[Double] { x: Double => x.toDouble }
  implicit val RationalIsReal = IsReal[Rational] { x: Rational => x.toDouble }
  implicit val RealIsReal = IsReal[Real] { x: Real => x.toDouble }
  implicit val BigDecimalIsReal = IsReal[BigDecimal] { x: BigDecimal => x.toDouble }
  implicit val SafeLongIsReal = IsReal[SafeLong] { x: SafeLong => x.toDouble }
  implicit val NaturalIsReal = IsReal[Natural] { x: Natural => x.toDouble }
}
