package spire
package math

import spire.algebra.{Conversion, CRing, Homomorphism}

import java.math.BigInteger

trait IsInZ[A] extends IsInQ[A] {
  def toRational(a: A): Rational = Rational(toSafeLong(a))
  def doubleApprox(a: A): Double = toSafeLong(a).toDouble
  def bigDecimalApprox(a: A, scale: Int): BigDecimal = toSafeLong(a).toBigDecimal
  def toSafeLong(a: A): SafeLong
  def toHomomorphism: Homomorphism[A, SafeLong, CRing] = new Conversion[A, SafeLong] {
    type Hom[X] = CRing[X]
    def apply(a: A): SafeLong = toSafeLong(a)
  }
}

trait IsZ[A] extends IsInZ[A] {
  def fromSafeLong(sl: SafeLong): A
  def toInverseHomomorphism: Homomorphism[SafeLong, A, CRing] = new Conversion[SafeLong, A] {
    type Hom[X] = CRing[X]
    def apply(sl: SafeLong): A = fromSafeLong(sl)
  }
}

object IsZ {

  def apply[A](implicit ev: IsZ[A]): IsZ[A] = ev

  def apply[A](f: A => SafeLong, g: SafeLong => A): IsZ[A] = new IsZ[A] {
    def toSafeLong(a: A) = f(a)
    def fromSafeLong(sl: SafeLong) = g(sl)
  }

  implicit val SafeLongIsZ: IsZ[SafeLong] = apply(identity, identity)

  implicit val BigIntIsZ: IsZ[BigInt] = apply(SafeLong(_), _.toBigInt)

  implicit val BigIntegerIsZ: IsZ[BigInteger] = apply(SafeLong(_), _.toBigInteger)

  implicit val LongIsZ: IsZ[Long] = apply(SafeLong(_), _.toLong)

  implicit val IntIsZ: IsZ[Int] = apply(SafeLong(_), _.toInt)

  implicit val ShortIsZ: IsZ[Short] = apply(SafeLong(_), _.toShort)

  implicit val ByteIsZ: IsZ[Byte] = apply(SafeLong(_), _.toByte)

}

class IsZOps[A](lhs: A)(implicit ev: IsZ[A]) {
  def toSafeLong: SafeLong = ev.toSafeLong(lhs)
  private def toLong(a: A): Long = {
    val sl = ev.toSafeLong(a)
    assert(sl.isValidLong)
    sl.toLong
  }
  private def fromBigInt(bi: BigInt): A = ev.fromSafeLong(SafeLong(bi))
  def isPrime: Boolean = prime.isPrime(ev.toSafeLong(lhs))
  def ! : A = fromBigInt(spire.math.fact(toLong(lhs)))
  def choose(rhs: A): A = fromBigInt(spire.math.choose(toLong(lhs), toLong(rhs)))
}
