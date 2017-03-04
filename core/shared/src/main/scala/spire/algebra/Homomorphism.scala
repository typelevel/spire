package spire.algebra

import java.math.BigInteger

import spire.math.{Complex, Rational, SafeLong}

trait Homomorphism[-A, +B, +F[_]] { self =>

  def apply(a: A): B

  def toFunction: Function1[A, B] = new Function1[A, B] {
    def apply(a: A): B = self.apply(a)
  }

}

abstract class Homomorphism0 {

  implicit def safeLongToCRing[A:CRing]: Homomorphism[SafeLong, A, CRing] =
    new Homomorphism[SafeLong, A, CRing] {
      def apply(a: SafeLong): A = CRing.fromBigInt(a.toBigInt)
    }

  implicit def bigIntToCRing[A:CRing]: Homomorphism[BigInt, A, CRing] =
    new Homomorphism[BigInt, A, CRing] {
      def apply(a: BigInt): A = CRing.fromBigInt(a)
    }

}

object Homomorphism extends Homomorphism0 {

  def apply[A, B, F[_]](f: A => B)(implicit ev1: F[A], ev2: F[B]): Homomorphism[A, B, F] =
    new Homomorphism[A, B, F] {
      override def toFunction = f
      def apply(a: A): B = f(a)
    }

  implicit def identity[A, F[_]](implicit ev: F[A]): Homomorphism[A, A, F] =
    new Homomorphism[A, A, F] {
      def apply(a: A): A = a
    }

  // a few instances

  implicit object safeLongToBigInt extends Homomorphism[SafeLong, BigInt, EuclideanRing] {
    def apply(a: SafeLong): BigInt = a.toBigInt
  }

  implicit object bigIntToSafeLong extends Homomorphism[BigInt, SafeLong, EuclideanRing] {
    def apply(a: BigInt): SafeLong = SafeLong(a)
  }

  implicit object safeLongToBigInteger extends Homomorphism[SafeLong, BigInteger, EuclideanRing] {
    def apply(a: SafeLong): BigInteger = a.toBigInt.bigInteger
  }

  implicit object bigIntToBigInteger extends Homomorphism[BigInt, BigInteger, EuclideanRing] {
    def apply(a: BigInt): BigInteger = a.bigInteger
  }

  implicit object bigIntegerToSafeLong extends Homomorphism[BigInteger, SafeLong, EuclideanRing] {
    def apply(a: BigInteger): SafeLong = SafeLong(a)
  }

  implicit object bigIntegerToBigInt extends Homomorphism[BigInteger, BigInt, EuclideanRing] {
    def apply(a: BigInteger): BigInt = BigInt(a)
  }

  implicit def realToComplex[A:Field]: Homomorphism[A, Complex[A], Field] =
    new Homomorphism[A, Complex[A], Field] {
      def apply(a: A): Complex[A] = Complex(a)
    }

  // some examples

  implicit def integerToRational[A](implicit ev: Homomorphism[A, SafeLong, EuclideanRing]): Homomorphism[A, Rational, CRing] =
    new Homomorphism[A, Rational, CRing] {
      def apply(a: A): Rational = Rational(ev(a))
    }

}
