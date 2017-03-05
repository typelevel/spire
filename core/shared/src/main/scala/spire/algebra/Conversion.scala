package spire.algebra

import spire.math.{Complex, IsInQ, IsInR, IsZ}

/** Conversion from type A to type B, also known as a homomorphism.
  *
  * Note: we discourage heavily the creation of implicit functions that
  * transform instances of Homomorphism into other instances of Homomorphism.
  * We use more precise types for the pieces that build up a `Conversion`.
  */
trait Conversion[A, B] { self =>

  /** Eventual algebraic structure preserved by the conversion/homomorphism. */
  type Hom[_]

  def apply(a: A): B

  def toFunction: Function1[A, B] = new Function1[A, B] {
    def apply(a: A): B = self.apply(a)
  }

  // TODO: could provide a false positive if we consider conversion between Byte and UByte
  def isValid(a: A)(implicit equ: Eq[A], inv: Conversion[B, A]): Boolean = equ.eqv(inv.apply(apply(a)), a)

}

abstract class Conversion0 {

  implicit def throughSafeLongToRing[A, B](implicit A: IsZ[A], B: Ring[B]): Homomorphism[A, B, Ring] =
    new Conversion[A, B] {
      type Hom[X] = Ring[X]
      def apply(a: A): B = B.fromBigInt(A.toSafeLong(a).toBigInt)
    }

  implicit def realToComplexCRing[A:CRing]: Homomorphism[A, Complex[A], CRing] =
    new Conversion[A, Complex[A]] {
      type Hom[X] = CRing[X]
      def apply(a: A): Complex[A] = Complex(a)
    }


}

abstract class Conversion1 extends Conversion0 {


  implicit def throughSafeLong[A, B](implicit A: IsZ[A], B: IsZ[B]): Homomorphism[A, B, CRing] =
    new Conversion[A, B] {
      type Hom[X] = CRing[X]
      def apply(a: A): B = B.fromSafeLong(A.toSafeLong(a))
    }

  implicit def throughSafeLongToCRing[A, B](implicit A: IsZ[A], B: CRing[B]): Homomorphism[A, B, CRing] =
    new Conversion[A, B] {
      type Hom[X] = CRing[X]
      def apply(a: A): B = B.fromBigInt(A.toSafeLong(a).toBigInt)
    }

}

abstract class Conversion2 extends Conversion1 {

  implicit def throughRationalToField[A, B](implicit A: IsInQ[A], B: Field[B]): Conversion[A, B] =
    new Conversion[A, B] {
      def apply(a: A): B = {
        val r = A.toRational(a)
        B.div(B.fromBigInt(r.numerator.toBigInt), B.fromBigInt(r.denominator.toBigInt))
      }
    }

  implicit def throughRationalToDivisionRing[A, B](implicit A: IsInQ[A], B: DivisionRing[B]): Conversion[A, B] =
    new Conversion[A, B] {
      def apply(a: A): B = {
        val r = A.toRational(a)
        B.div(B.fromBigInt(r.numerator.toBigInt), B.fromBigInt(r.denominator.toBigInt))
      }
    }

}

object Conversion extends Conversion2 {

  implicit def approxToDouble[A](implicit A: IsInR[A]): Conversion[A, Double] =
    new Conversion[A, Double] {
      def apply(a: A): Double = A.doubleApprox(a)
    }

  implicit def approxToFloat[A](implicit A: IsInR[A]): Conversion[A, Float] =
    new Conversion[A, Float] {
      def apply(a: A): Float = A.doubleApprox(a).toFloat
    }

  /*
  implicit def realToComplexField[A:Field]: Homomorphism[A, Complex[A], Field] =
    new Conversion[A, Complex[A]] {
      type Hom[X] = Field[X]
      def apply(a: A): Complex[A] = Complex(a)
    }*/

}
