package spire
package math

import spire.algebra.{CRing, EuclideanRing, Homomorphism}
import spire.std.bigInt._
import spire.std.bigInteger._

import java.math.BigInteger

trait IsZ[A] {
  def toSafeLong: Homomorphism[A, SafeLong, EuclideanRing]
  def fromSafeLong: Homomorphism[SafeLong, A, EuclideanRing]
}

object IsZ {

  implicit object SafeLongIsZ extends IsZ[SafeLong] {
    val toSafeLong = Homomorphism.identity[SafeLong, EuclideanRing]
    val fromSafeLong = toSafeLong
  }

  implicit object BigIntIsZ extends IsZ[BigInt] {
    val toSafeLong = Homomorphism[BigInt, SafeLong, EuclideanRing](SafeLong(_))
    val fromSafeLong = Homomorphism[SafeLong, BigInt, EuclideanRing](_.toBigInt)
  }

  implicit object BigIntegerIsZ extends IsZ[BigInteger] {
    val toSafeLong = Homomorphism[BigInteger, SafeLong, EuclideanRing](SafeLong(_))
    val fromSafeLong = Homomorphism[SafeLong, BigInteger, EuclideanRing](_.toBigInteger)
  }

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
