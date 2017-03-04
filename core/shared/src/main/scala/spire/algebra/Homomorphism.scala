package spire.algebra

import java.math.BigInteger

import spire.NoImplicit
import spire.math._

/** Conversoin from type A to type {
  *
  *
  *
  * Note: we discourage heavily the creation of implicit functions that
  * transform instances of Homomorphism into other instance of Homomorphism.
  * Create subtypes for the intermediate transformations.
  */
trait Homomorphism[-A, +B, +F[_]] { self =>

  def apply(a: A): B

  def toFunction: Function1[A, B] = new Function1[A, B] {
    def apply(a: A): B = self.apply(a)
  }

}

abstract class Homomorphism0 {

  implicit def identity[A, F[_]](implicit ev: F[A]): Homomorphism[A, A, F] =
    new Homomorphism[A, A, F] {
      def apply(a: A): A = a
    }

  implicit def safeLongToCRing[A:CRing]: Homomorphism[SafeLong, A, CRing] =
    new Homomorphism[SafeLong, A, CRing] {
      def apply(a: SafeLong): A = CRing.fromBigInt(a.toBigInt)
    }

  implicit def bigIntToCRing[A:CRing]: Homomorphism[BigInt, A, CRing] =
    new Homomorphism[BigInt, A, CRing] {
      def apply(a: BigInt): A = CRing.fromBigInt(a)
    }

}

abstract class Homomorphism1 extends Homomorphism0 {

}

abstract class Homomorphism2 extends Homomorphism1 {

}

object Homomorphism extends Homomorphism2 {

  def apply[A, B, F[_]](f: A => B)(implicit ev1: F[A], ev2: F[B]): Homomorphism[A, B, F] =
    new Homomorphism[A, B, F] {
      override def toFunction = f
      def apply(a: A): B = f(a)
    }

  trait ToSafeLong[-A, +F[_]] extends Homomorphism[A, SafeLong, F]

  // instances from and to SafeLong for signed integers

  implicit object SafeLongToSafeLong extends ToSafeLong[SafeLong, CRing] {
    def apply(a: SafeLong): SafeLong = a
  }

  implicit object ByteToSafeLong extends ToSafeLong[Byte, CRing] {
    def apply(a: Byte): SafeLong = SafeLong(a)
  }

  implicit object ShortToSafeLong extends ToSafeLong[Short, CRing] {
    def apply(a: Short): SafeLong = SafeLong(a)
  }

  implicit object IntToSafeLong extends ToSafeLong[Int, CRing] {
    def apply(a: Int): SafeLong = SafeLong(a)
  }

  implicit object LongToSafeLong extends ToSafeLong[Long, CRing] {
    def apply(a: Long): SafeLong = SafeLong(a)
  }

  implicit object BigIntToSafeLong extends ToSafeLong[BigInt, CRing] {
    def apply(a: BigInt): SafeLong = SafeLong(a)
  }

  implicit object BigIntegerToSafeLong extends ToSafeLong[BigInteger, CRing] {
    def apply(a: BigInteger): SafeLong = SafeLong(a)
  }

  implicit object UByteToSafeLong extends ToSafeLong[UByte, CRig] {
    def apply(a: UByte): SafeLong = SafeLong(a.toLong)
  }

  implicit object UShortToSafeLong extends ToSafeLong[UShort, CRig] {
    def apply(a: UShort): SafeLong = SafeLong(a.toLong)
  }

  implicit object UIntToSafeLong extends ToSafeLong[UInt, CRig] {
    def apply(a: UInt): SafeLong = SafeLong(a.toLong)
  }

  implicit object ULongToSafeLong extends ToSafeLong[ULong, CRig] {
    def apply(a: ULong): SafeLong = SafeLong(a)
  }

  implicit def toByte[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, Byte, F] =
    new Homomorphism[A, Byte, F] {
      def apply(a: A): Byte = A(a).toByte
    }

  implicit def toShort[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, Short, F] =
    new Homomorphism[A, Short, F] {
      def apply(a: A): Short = A(a).toShort
    }

  implicit def toInt[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, Int, F] =
    new Homomorphism[A, Int, F] {
      def apply(a: A): Int = A(a).toInt
    }

  implicit def toLong[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, Long, F] =
    new Homomorphism[A, Long, F] {
      def apply(a: A): Long = A(a).toLong
    }

  implicit def toBigInt[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, BigInt, F] =
    new Homomorphism[A, BigInt, F] {
      def apply(a: A): BigInt = A(a).toBigInt
    }

  implicit def toBigInteger[A, F[X] <: CRig[X]](implicit A: ToSafeLong[A, F]): Homomorphism[A, BigInteger, F] =
    new Homomorphism[A, BigInteger, F] {
      def apply(a: A): BigInteger = A(a).toBigInt.bigInteger
    }

  // instances from and to ULong for unsigned integers

  trait ToULong[-A, +F[_]] extends Homomorphism[A, ULong, F]

  implicit object ULongToULong extends ToULong[ULong, CRig] {
    def apply(a: ULong): ULong = a
  }

  implicit object UByteToULong extends ToULong[UByte, CRig] {
    def apply(a: UByte): ULong = ULong(a.toLong)
  }

  implicit object UShortToULong extends ToULong[UShort, CRig] {
    def apply(a: UShort): ULong = ULong(a.toLong)
  }

  implicit object UIntToULong extends ToULong[UInt, CRig] {
    def apply(a: UInt): ULong = ULong(a.toLong)
  }

  implicit def toUByte[A](implicit A: ToULong[A, CRig]): Homomorphism[A, UByte, CRig] =
    new Homomorphism[A, UByte, CRig] {
      def apply(a: A): UByte = UByte(A(a).toInt)
    }

  implicit def toUShort[A](implicit A: ToULong[A, CRig]): Homomorphism[A, UShort, CRig] =
    new Homomorphism[A, UShort, CRig] {
      def apply(a: A): UShort = UShort(A(a).toInt)
    }

  implicit def toUInt[A](implicit A: ToULong[A, CRig]): Homomorphism[A, UInt, CRig] =
    new Homomorphism[A, UInt, CRig] {
      def apply(a: A): UInt = UInt(A(a).toLong)
    }

  implicit def realToComplex[A:Field]: Homomorphism[A, Complex[A], Field] =
    new Homomorphism[A, Complex[A], Field] {
      def apply(a: A): Complex[A] = Complex(a)
    }

  /*
  // some examples

  implicit def integerToRational[A](implicit ev: Homomorphism[A, SafeLong, CRing]): Homomorphism[A, Rational, CRing] =
    new Homomorphism[A, Rational, CRing] {
      def apply(a: A): Rational = Rational(ev(a))
    }*/

}
